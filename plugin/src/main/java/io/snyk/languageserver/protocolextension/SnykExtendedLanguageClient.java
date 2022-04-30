package io.snyk.languageserver.protocolextension;

import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4e.ServerMessageHandler;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;

@SuppressWarnings("restriction")
public class SnykExtendedLanguageClient extends LanguageClientImpl {

  private static final Preferences PREFERENCES = new Preferences();

  @JsonNotification(value = "$/hasAuthenticated")
  public void hasAuthenticated(HasAuthenticatedParam param) {
    PREFERENCES.store(Preferences.AUTH_TOKEN_KEY, param.getToken());
    MessageParams messageParams = new MessageParams();
    messageParams.setType(MessageType.Info);
    messageParams.setMessage("The authentication token has been stored in Snyk Preferences.");
    ServerMessageHandler.showMessage("Authentication with Snyk successful", messageParams);
  }
}
