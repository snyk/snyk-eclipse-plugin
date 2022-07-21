package io.snyk.languageserver.protocolextension;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4e.ServerMessageHandler;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.ui.PlatformUI;

import java.util.concurrent.CompletableFuture;

@SuppressWarnings("restriction")
public class SnykExtendedLanguageClient extends LanguageClientImpl {
  private ProgressManager progressMgr = new ProgressManager();

  @SuppressWarnings("unused") // used in lsp4e language server instantiation
  public SnykExtendedLanguageClient() {
    super();
  }

  public SnykExtendedLanguageClient(ProgressManager pm) {
    this.progressMgr = pm;
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
      if (snykView != null) snykView.toggleRunActionEnablement();
    });
  }

  private void showAuthenticatedMessage() {
    MessageParams messageParams = new MessageParams();
    messageParams.setType(MessageType.Info);
    messageParams.setMessage("The authentication token has been stored in Snyk Preferences.");
    ServerMessageHandler.showMessage("Authentication with Snyk successful", messageParams);
  }
}
