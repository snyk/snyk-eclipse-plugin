package io.snyk.languageserver;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4j.DidChangeWorkspaceFoldersParams;
import org.eclipse.lsp4j.WorkspaceFolder;
import org.eclipse.lsp4j.WorkspaceFoldersChangeEvent;
import org.eclipse.lsp4j.services.LanguageServer;

import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

/**
 * Listens to workspace project add / remove / open / close events and pushes a
 * workspace/didChangeWorkspaceFolders notification to the Snyk language server
 * when the set of top-level accessible projects changes. Without this, the LS
 * keeps scanning the workspace folders it was told about at initialization
 * time, so newly imported projects are ignored and removed projects keep
 * producing stale results.
 */
@SuppressWarnings("restriction")
public final class WorkspaceFolderChangeTracker implements IResourceChangeListener {

    private static final int CHANGE_MASK = IResourceChangeEvent.POST_CHANGE;

    private static volatile WorkspaceFolderChangeTracker instance;

    // Keyed by URI string for a stable identity that survives IProject re-creation.
    private final Map<String, WorkspaceFolder> knownFolders = new HashMap<>();
    private final Object lock = new Object();
    private final Supplier<Map<String, WorkspaceFolder>> folderSource;
    private final BiFunction<List<WorkspaceFolder>, List<WorkspaceFolder>, Boolean> sender;

    private WorkspaceFolderChangeTracker() {
        this(WorkspaceFolderChangeTracker::currentFolderMap, WorkspaceFolderChangeTracker::sendDidChange);
    }

    // Package-private for testing.
    WorkspaceFolderChangeTracker(
            Supplier<Map<String, WorkspaceFolder>> folderSource,
            BiFunction<List<WorkspaceFolder>, List<WorkspaceFolder>, Boolean> sender) {
        this.folderSource = folderSource;
        this.sender = sender;
        snapshot();
    }

    public static synchronized void register() {
        if (instance != null) return;
        WorkspaceFolderChangeTracker tracker = new WorkspaceFolderChangeTracker();
        ResourcesPlugin.getWorkspace().addResourceChangeListener(tracker, CHANGE_MASK);
        instance = tracker;
    }

    public static synchronized void unregister() {
        if (instance == null) return;
        ResourcesPlugin.getWorkspace().removeResourceChangeListener(instance);
        instance = null; // NOPMD - intentional singleton reset
    }

    @Override
    public void resourceChanged(IResourceChangeEvent event) {
        try {
            if (!affectsProjectSet(event.getDelta())) return;
            recomputeAndNotify();
        } catch (Exception e) { // NOPMD - listener must not throw back into Eclipse
            SnykLogger.logError(e);
        }
    }

    // POST_CHANGE fires for every resource event (file saves, compilations, etc).
    // Only proceed when top-level projects are added, removed, or opened/closed.
    // Visible for testing.
    static boolean affectsProjectSet(IResourceDelta rootDelta) {
        if (rootDelta == null) return false;
        for (IResourceDelta delta : rootDelta.getAffectedChildren()) {
            int kind = delta.getKind();
            if (kind == IResourceDelta.ADDED || kind == IResourceDelta.REMOVED) return true;
            // IResourceDelta.OPEN fires for both open and close: the flag means the
            // project's open state changed, not that it was opened. Closing a project
            // sets this flag too, which is counterintuitive but matches Eclipse behaviour.
            if ((delta.getFlags() & IResourceDelta.OPEN) != 0) return true;
        }
        return false;
    }

    // Package-private for testing.
    void recomputeAndNotify() {
        List<WorkspaceFolder> added;
        List<WorkspaceFolder> removed;
        Map<String, WorkspaceFolder> currentFolders;
        synchronized (lock) {
            currentFolders = folderSource.get();
            added = new ArrayList<>();
            for (Map.Entry<String, WorkspaceFolder> e : currentFolders.entrySet()) {
                if (!knownFolders.containsKey(e.getKey())) added.add(e.getValue());
            }
            removed = new ArrayList<>();
            for (Map.Entry<String, WorkspaceFolder> e : knownFolders.entrySet()) {
                if (!currentFolders.containsKey(e.getKey())) removed.add(e.getValue());
            }
            if (added.isEmpty() && removed.isEmpty()) {
                return;
            }
        }
        if (sender.apply(added, removed)) {
            synchronized (lock) {
                knownFolders.clear();
                knownFolders.putAll(currentFolders);
            }
        }
    }

    private void snapshot() {
        synchronized (lock) {
            knownFolders.clear();
            knownFolders.putAll(folderSource.get());
        }
    }

    private static Map<String, WorkspaceFolder> currentFolderMap() {
        Map<String, WorkspaceFolder> map = new HashMap<>();
        for (IProject project : ResourceUtils.getAccessibleTopLevelProjects()) {
            WorkspaceFolder folder = LSPEclipseUtils.toWorkspaceFolder(project);
            if (folder != null && folder.getUri() != null) {
                map.put(folder.getUri(), folder);
            }
        }
        return map;
    }

    private static boolean sendDidChange(List<WorkspaceFolder> added, List<WorkspaceFolder> removed) {
        SnykExtendedLanguageClient client = SnykExtendedLanguageClient.getInstance();
        if (client == null) {
            return false;
        }
        LanguageServer ls = client.getConnectedLanguageServer();
        if (ls == null) {
            return false;
        }
        WorkspaceFoldersChangeEvent change = new WorkspaceFoldersChangeEvent(added, removed);
        DidChangeWorkspaceFoldersParams params = new DidChangeWorkspaceFoldersParams(change);
        try {
            ls.getWorkspaceService().didChangeWorkspaceFolders(params);
            return true;
        } catch (Exception e) { // NOPMD - lsp4j can throw on transport errors
            SnykLogger.logError(e);
            return false;
        }
    }
}
