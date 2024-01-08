package io.snyk.languageserver.protocolextension;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.SnykView;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.OAuthToken;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;


@SuppressWarnings("restriction")
public class SnykExtendedLanguageClient extends LanguageClientImpl {
    private final ProgressManager progressMgr = new ProgressManager();
    private static SnykExtendedLanguageClient instance = null;
    private final ObjectMapper om = new ObjectMapper();

    @SuppressWarnings("unused") // used in lsp4e language server instantiation
    public SnykExtendedLanguageClient() {
        super();
        instance = this;
    }

    public static SnykExtendedLanguageClient getInstance() {
        return instance; // we leave instantiation to LSP4e, no lazy construction here
    }

    public LanguageServer getConnectedLanguageServer() {
        return super.getLanguageServer();
    }


    public void triggerScan(IWorkbenchWindow window) {
        if (Preferences.getInstance().getAuthToken().isBlank()) {
            runSnykWizard();
        } else {
            try {
                if (window == null) {
                    executeCommand("snyk.workspace.scan", new ArrayList<>());
                    return;
                }

                ISelectionService service = window.getSelectionService();
                IStructuredSelection structured = (IStructuredSelection) service.getSelection();

                Object firstElement = structured.getFirstElement();
                IProject project = null;
                if (firstElement instanceof JavaProject) {
                    project = ((JavaProject) firstElement).getProject();
                }

                if (firstElement instanceof IProject) {
                    project = (IProject) firstElement;
                }

                if (project != null) {
                    runForProject(project.getName());
                    executeCommand("snyk.workspaceFolder.scan", List.of(project.getLocation().toOSString()));
                }
            } catch (Exception e) {
                SnykLogger.logError(e);
            }

        }
    }

    public void triggerAuthentication() {
        executeCommand("snyk.login", new ArrayList<>());
    }

    public void trustWorkspaceFolders() {
        executeCommand("snyk.trustWorkspaceFolders", new ArrayList<>());
    }

    public boolean getSastEnabled() {
        ExecuteCommandParams params = new ExecuteCommandParams("snyk.getSettingsSastEnabled", new ArrayList<>());
        try {
            CompletableFuture<Object> lsSastSettings = getConnectedLanguageServer().getWorkspaceService().executeCommand(params);
            ObjectMapper mapper = new ObjectMapper();
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            SastSettings sastSettings = mapper.convertValue(lsSastSettings.get(), SastSettings.class);
            return sastSettings != null ? sastSettings.sastEnabled : false;
        } catch (Exception e) {
            // ignore
        }

        return false;
    }

    @JsonNotification(value = "$/snyk.hasAuthenticated")
    public void hasAuthenticated(HasAuthenticatedParam param) {
        var prefs = Preferences.getInstance();
        prefs.store(Preferences.AUTH_TOKEN_KEY, param.getToken());
        triggerScan(null);

        if (!param.getToken().isBlank()) {
            showAuthenticatedMessage();
            enableSnykViewRunActions();
        }

        setAuthenticationMethod(param, prefs);
    }

    @JsonNotification(value = "$/snyk.isAvailableCli")
    public void isAvailableCli(SnykIsAvailableCliParams param) {
        Preferences.getInstance().store(Preferences.CLI_PATH, param.getCliPath());
        enableSnykViewRunActions();
    }

    @JsonNotification(value = "$/snyk.addTrustedFolders")
    public void addTrustedPaths(SnykTrustedFoldersParams param) {
        var prefs = Preferences.getInstance();
        var storedTrustedPaths = prefs.getPref(Preferences.TRUSTED_FOLDERS, "");
        var trustedPaths = storedTrustedPaths.split(File.pathSeparator);
        var pathSet = new HashSet<>(Arrays.asList(trustedPaths));
        pathSet.addAll(Arrays.asList(param.getTrustedFolders()));
        Preferences.getInstance().store(Preferences.TRUSTED_FOLDERS, pathSet.stream().filter(s -> !s.isBlank())
            .map(s -> s.trim()).distinct().collect(Collectors.joining(File.pathSeparator)));
    }

    @Override
    public CompletableFuture<Void> createProgress(WorkDoneProgressCreateParams params) {
        return progressMgr.createProgress(params);
    }

    @Override
    public void notifyProgress(ProgressParams params) {
        progressMgr.updateProgress(params);
    }

    private void runSnykWizard() {
        SnykWizard wizard = new SnykWizard();

        WizardDialog dialog = new WizardDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard);

        dialog.setBlockOnOpen(true);
        dialog.open();
    }

    private void enableSnykViewRunActions() {
        PlatformUI.getWorkbench().getDisplay().asyncExec(() -> {
            var snykView = SnykStartup.getSnykView();
            if (snykView != null)
                snykView.toggleRunActionEnablement();
        });
    }

    private void showAuthenticatedMessage() {
        MessageParams messageParams = new MessageParams();
        messageParams.setType(MessageType.Info);
        messageParams.setMessage("The authentication token has been stored in Snyk Preferences.");
        super.showMessage(messageParams);
    }

    private void runForProject(String projectName) {
        SnykView snykView = SnykStartup.getSnykView();
        if (snykView != null) {
            snykView.testProject(projectName);
        }
    }

    private void executeCommand(@NonNull String command, List<Object> arguments) {
        ExecuteCommandParams params = new ExecuteCommandParams(command, arguments);
        try {
            getConnectedLanguageServer().getWorkspaceService().executeCommand(params);
        } catch (Exception e) {
            SnykLogger.logError(e);
        }
    }

    protected void setAuthenticationMethod(HasAuthenticatedParam param, Preferences prefs) {
        // check if its a json token and store the auth method based on that
        try {
            om.readValue(param.getToken(), OAuthToken.class);
            prefs.store(Preferences.AUTHENTICATION_METHOD, Preferences.AUTH_METHOD_OAUTH);
        } catch (JsonProcessingException e) {
            prefs.store(Preferences.AUTHENTICATION_METHOD, Preferences.AUTH_METHOD_TOKEN);
        }
    }

    /**
     * Refresh the token using language server. Waits up to 2s for the token change.
     *
     * @return true if token has changed, false if not
     */
    public boolean refreshOAuthToken() {
        var p = Preferences.getInstance();
        var token = p.getAuthToken();
        executeCommand("snyk.getActiveUser", new ArrayList<>());
        // wait until token has changed or 2s have passed
        CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
            while (token.equals(p.getAuthToken())) {
                try {
                    Thread.sleep(10);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
            return p.getAuthToken();
        });
        var newToken = future.completeOnTimeout(token, 2, TimeUnit.SECONDS).join();
        return !token.equals(newToken);
    }

    static class SastSettings {
        public boolean sastEnabled;

        public LocalCodeEngine localCodeEngine;

        public boolean reportFalsePositivesEnabled;

        public String org;

        public boolean autofixEnabled;
    }

    /**
     * SAST local code engine configuration.
     */
    static class LocalCodeEngine {
        public boolean enabled;

        public String url;
    }

    public static <T> T convertInstanceOfObject(Object o, Class<T> clazz) {
        try {
            return clazz.cast(o);
        } catch (ClassCastException e) {
            return null;
        }
    }
}
