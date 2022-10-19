package io.snyk.languageserver.protocolextension;

import static io.snyk.eclipse.plugin.utils.SnykLogger.logError;

import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4e.ServerMessageHandler;
import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.ShowDocumentParams;
import org.eclipse.lsp4j.ShowDocumentResult;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.LsRuntimeEnvironment;
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;

@SuppressWarnings("restriction")
public class SnykExtendedLanguageClient extends LanguageClientImpl {
  private final ProgressManager progressMgr = new ProgressManager();
  private static SnykExtendedLanguageClient instance = null;

  @SuppressWarnings("unused") // used in lsp4e language server instantiation
  public SnykExtendedLanguageClient() {
    super();
    instance = this;
  }

  public static SnykExtendedLanguageClient getInstance() {
    return instance; // we leave instantiation to LSP4e, no lazy construction here
  }

  public void triggerScan() {
    PlatformUI.getWorkbench().getDisplay().asyncExec(() -> {
      SnykWizard wizard = new SnykWizard();

      WizardDialog dialog = new WizardDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard);

      if (Preferences.getInstance().getAuthToken().isBlank()) {
        dialog.setBlockOnOpen(true);
        dialog.open();
      }
      ExecuteCommandParams params = new ExecuteCommandParams("snyk.workspace.scan", new ArrayList<>());
      try {
        getLanguageServer().getWorkspaceService().executeCommand(params);
      } catch (Exception e) {
        SnykLogger.logError(e);
      }
    });
  }

  public void triggerAuthentication() {
    ExecuteCommandParams params = new ExecuteCommandParams("snyk.login", new ArrayList<>());
    try {
      getLanguageServer().getWorkspaceService().executeCommand(params);
    } catch (Exception e) {
      SnykLogger.logError(e);
    }
  }

  @JsonNotification(value = "$/snyk.hasAuthenticated")
  public void hasAuthenticated(HasAuthenticatedParam param) {
    Preferences.getInstance().store(Preferences.AUTH_TOKEN_KEY, param.getToken());
    showAuthenticatedMessage();
    enableSnykViewRunActions();
  }

  @JsonNotification(value = "$/snyk.isAvailableCli")
  public void isAvailableCli(SnykIsAvailableCliParams param) {
    Preferences.getInstance().store(Preferences.CLI_PATH, param.getCliPath());
    enableSnykViewRunActions();
  }

  @Override
  public CompletableFuture<Void> createProgress(WorkDoneProgressCreateParams params) {
    return progressMgr.createProgress(params);
  }

  @Override
  public void notifyProgress(ProgressParams params) {
    progressMgr.updateProgress(params);
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
    ServerMessageHandler.showMessage("Authentication with Snyk successful", messageParams);
  }

  // TODO: remove once LSP4e supports `showDocument` in its next release (it's
  // been merged to it already)
  @Override
  public CompletableFuture<ShowDocumentResult> showDocument(ShowDocumentParams params) {
    return CompletableFuture.supplyAsync(() -> {
      PlatformUI.getWorkbench().getDisplay().syncExec(() -> {
        var location = new Location(params.getUri(), params.getSelection());
        var window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
        if (window != null) {
          var page = window.getActivePage();
          LSPEclipseUtils.openInEditor(location, page);
        }
      });
      return new ShowDocumentResult(true);
    });
  }
}
