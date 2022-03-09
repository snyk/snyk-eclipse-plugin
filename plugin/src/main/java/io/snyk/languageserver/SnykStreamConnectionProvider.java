package io.snyk.languageserver;

import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.download.LsDownloader;
import org.apache.commons.lang3.SystemUtils;
import org.apache.http.impl.client.HttpClients;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4e.LanguageServerWrapper;
import org.eclipse.lsp4e.LanguageServersRegistry;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

public class SnykStreamConnectionProvider extends ProcessStreamConnectionProvider implements StreamConnectionProvider, IJobChangeListener {
    public static final String LANGUAGE_SERVER_ID = "io.snyk.languageserver";
    private final LsUtils utils = new LsUtils(new Preferences());
    private static boolean downloadStarted = false;
    private static boolean initialized = false;

    public SnykStreamConnectionProvider() {
        if (!downloadStarted && !initialized) {
            if (update()) {
                downloadStarted = true;
                Job downloadLSP = getJob();
                downloadLSP.setPriority(Job.LONG);
                downloadLSP.addJobChangeListener(this);
                downloadLSP.schedule();
            } else {
                initialized = true;
            }
        }
    }

    private boolean update() {
        File lsFile = utils.getLSFile();
        boolean customCliPath = new Preferences().getLsBinary() == null;
        if (customCliPath) return false;
        if (lsFile.exists()) {
            try {
                BasicFileAttributes basicFileAttributes;
                basicFileAttributes = Files.readAttributes(lsFile.toPath(), BasicFileAttributes.class);
                Instant lastModified = basicFileAttributes.lastModifiedTime().toInstant();
                return lastModified.isBefore(Instant.now().minus(4, ChronoUnit.DAYS));
            } catch (IOException e) {
                SnykLogger.logError(e);
                return false;
            }
        }
        return true;
    }

    @Override
    public void start() throws IOException {
        if (!initialized || downloadStarted) throw new IllegalStateException("Not yet initialized!");
        List<String> commands = Lists.of(utils.getLSFile().getCanonicalPath());
        String workingDir = SystemUtils.USER_DIR;
        setCommands(commands);
        setWorkingDirectory(workingDir);
        super.start();
    }

    private Job getJob() {
        return new Job("Downloading latest Snyk LSP ...") {
            private final File lsFile = utils.getLSFile();

            @SuppressWarnings("ResultOfMethodCallIgnored")
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    var httpClient = HttpClients.createDefault();
                    var lsDownloader = new LsDownloader(utils, httpClient);
                    lsFile.getParentFile().mkdirs();
                    lsDownloader.download(monitor);
                    lsFile.setExecutable(true);
                } catch (RuntimeException e) {
                    return Status.error("Download of Snyk Language Server failed", e);
                }
                return Status.OK_STATUS;
            }
        };
    }

    @Override
    public void done(IJobChangeEvent iJobChangeEvent) {
        downloadStarted = false;
        initialized = true;
        // reinitialize the language server after download
        var definition = LanguageServersRegistry.getInstance().getDefinition(LANGUAGE_SERVER_ID);
        if (definition != null) {
            IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
            getIResources(windows).forEach(r -> registerWithLanguageServer(r, definition));
        }
    }

    private static Set<IResource> getIResources(IWorkbenchWindow[] ww) {
        return Arrays.stream(ww).map(w -> {
            Set<IFile> files = new HashSet<>();
            IWorkbenchPage activePage = w.getActivePage();
            if (activePage != null) {
                IEditorPart editor = activePage.getActiveEditor();
                if (editor != null) {
                    files.add(LSPEclipseUtils.getFile(LSPEclipseUtils.getDocument(editor.getEditorInput())));
                }
            }
            return files;
        }).flatMap(Collection::stream).collect(Collectors.toUnmodifiableSet());
    }

    private static void registerWithLanguageServer(IResource resource, LanguageServersRegistry.LanguageServerDefinition definition) {
        new Job("Registering " + resource.getName() + " with Snyk") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    if (resource.isAccessible()) {
                        new LanguageServerWrapper(definition, resource.getFullPath()).start();
                        resource.touch(monitor);
                    }
                } catch (CoreException e) {
                    return Status.error("Couldn't touch resource " + resource, e);
                } catch (IOException e) {
                    return Status.error("Couldn't start Language server for resource " + resource, e);
                }
                return Status.OK_STATUS;
            }
        }.schedule();
    }

    @Override
    public void aboutToRun(IJobChangeEvent iJobChangeEvent) {
    }

    @Override
    public void awake(IJobChangeEvent iJobChangeEvent) {
    }

    @Override
    public void running(IJobChangeEvent iJobChangeEvent) {
    }

    @Override
    public void scheduled(IJobChangeEvent iJobChangeEvent) {
    }

    @Override
    public void sleeping(IJobChangeEvent iJobChangeEvent) {
    }
}
