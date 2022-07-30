package io.snyk.languageserver.protocolextension;

import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4e.ServerMessageHandler;
import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.ShowDocumentParams;
import org.eclipse.lsp4j.ShowDocumentResult;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;

@SuppressWarnings("restriction")
public class SnykExtendedLanguageClient extends LanguageClientImpl {
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
    ExecuteCommandParams params = new ExecuteCommandParams("snyk.workspace.scan", new ArrayList<Object>());
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

  // TODO: remove once LSP4e supports `showDocument` in its next release (it's been merged to it already)
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
