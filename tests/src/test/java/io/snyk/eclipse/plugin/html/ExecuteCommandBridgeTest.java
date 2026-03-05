package io.snyk.eclipse.plugin.html;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.LsBaseTest;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;

class ExecuteCommandBridgeTest extends LsBaseTest {

  @Test
  void buildClientScript_containsIdeExecuteCommandDefinition() {
    String script = ExecuteCommandBridge.buildClientScript();
    assertTrue(script.contains("window.__ideExecuteCommand__"),
        "Should define window.__ideExecuteCommand__");
  }

  @Test
  void buildClientScript_containsIdeCallbacksInitialization() {
    String script = ExecuteCommandBridge.buildClientScript();
    assertTrue(script.contains("window.__ideCallbacks__"),
        "Should initialize window.__ideCallbacks__");
  }

  @Test
  void buildClientScript_callsBridgeFunction() {
    String script = ExecuteCommandBridge.buildClientScript();
    assertTrue(script.contains("__ideExecuteCommandBridge__"),
        "Should call __ideExecuteCommandBridge__ native function");
  }

  @Test
  void buildClientScript_handlesCallbackRegistration() {
    String script = ExecuteCommandBridge.buildClientScript();
    assertTrue(script.contains("callback"), "Should register callback");
    assertTrue(script.contains("callbackId"), "Should use callbackId");
  }

  @Test
  void injectScript_doesNotThrowWhenBrowserIsNull() {
    // should not throw
    ExecuteCommandBridge.injectScript(null);
  }

  @Test
  void saveLoginArgs_savesOauthAuthMethod() {
    List<Object> args = Arrays.asList("oauth", "https://api.snyk.io", false);

    ExecuteCommandBridge.saveLoginArgs(args);

    assertEquals(AuthConstants.AUTH_OAUTH2, prefs.getPref(Preferences.AUTHENTICATION_METHOD, ""));
  }

  @Test
  void saveLoginArgs_savesPatAuthMethod() {
    List<Object> args = Arrays.asList("pat", "https://api.snyk.io", false);

    ExecuteCommandBridge.saveLoginArgs(args);

    assertEquals(AuthConstants.AUTH_PERSONAL_ACCESS_TOKEN,
        prefs.getPref(Preferences.AUTHENTICATION_METHOD, ""));
  }

  @Test
  void saveLoginArgs_savesTokenAuthMethod() {
    List<Object> args = Arrays.asList("token", "https://api.snyk.io", false);

    ExecuteCommandBridge.saveLoginArgs(args);

    assertEquals(AuthConstants.AUTH_API_TOKEN, prefs.getPref(Preferences.AUTHENTICATION_METHOD, ""));
  }

  @Test
  void saveLoginArgs_savesEndpoint() {
    List<Object> args = Arrays.asList("oauth", "https://api.eu.snyk.io", false);

    ExecuteCommandBridge.saveLoginArgs(args);

    assertEquals("https://api.eu.snyk.io", prefs.getPref(Preferences.ENDPOINT_KEY, ""));
  }

  @Test
  void saveLoginArgs_savesInsecure() {
    List<Object> args = Arrays.asList("oauth", "https://api.snyk.io", true);

    ExecuteCommandBridge.saveLoginArgs(args);

    assertEquals("true", prefs.getPref(Preferences.INSECURE_KEY, "false"));
  }
}
