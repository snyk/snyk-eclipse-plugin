package io.snyk.eclipse.plugin.html;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.CommandHandler;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;
import org.eclipse.swt.browser.ProgressAdapter;
import org.eclipse.swt.browser.ProgressEvent;
import org.eclipse.swt.widgets.Display;

/**
 * Shared bridge for the window.__ideExecuteCommand__ JS↔IDE contract. Usable by any SWT Browser
 * panel (settings preference page, tree view, etc.).
 *
 * <p>Responsibilities:
 *
 * <ul>
 *   <li>Register the native {@code __ideExecuteCommandBridge__} BrowserFunction that receives calls
 *       from JavaScript.
 *   <li>Inject the client-side JS wrapper that defines {@code window.__ideExecuteCommand__}.
 *   <li>Dispatch commands to the Language Server and return results via the JS callback registry.
 * </ul>
 */
public class ExecuteCommandBridge {

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  /**
   * Registers the {@code __ideExecuteCommandBridge__} BrowserFunction and adds a ProgressListener
   * that injects the client-side JS wrapper after each page load.
   */
  public static void install(Browser browser) {
    registerBridgeFunction(browser);
    browser.addProgressListener(
        new ProgressAdapter() {
          @Override
          public void completed(ProgressEvent event) {
            injectScript(browser);
          }
        });
  }

  /**
   * Returns the client-side JavaScript that defines {@code window.__ideExecuteCommand__} in the
   * browser. Assumes {@code __ideExecuteCommandBridge__} BrowserFunction is registered.
   */
  public static String buildClientScript() {
    return "(function() {"
        + "  if (window.__ideCallbacks__) { return; }"
        + "  window.__ideCallbacks__ = {};"
        + "  var __cbCounter = 0;"
        + "  window.__ideExecuteCommand__ = function(command, args, callback) {"
        + "    var callbackId = '';"
        + "    if (typeof callback === 'function') {"
        + "      callbackId = '__cb_' + (++__cbCounter);"
        + "      window.__ideCallbacks__[callbackId] = callback;"
        + "    }"
        + "    __ideExecuteCommandBridge__(command, JSON.stringify(args || []), callbackId);"
        + "  };"
        + "})();";
  }

  /**
   * Injects the client-side JS wrapper into the browser. Safe to call on any page load; the script
   * is idempotent (guarded by {@code window.__ideCallbacks__} check).
   */
  public static void injectScript(Browser browser) {
    if (browser == null || browser.isDisposed()) {
      return;
    }
    browser.execute(buildClientScript());
  }

  static void saveLoginArgs(List<Object> args) {
    String authMethodStr = String.valueOf(args.get(0));
    String authMethod;
    switch (authMethodStr) {
      case "pat":
        authMethod = AuthConstants.AUTH_PERSONAL_ACCESS_TOKEN;
        break;
      case "token":
        authMethod = AuthConstants.AUTH_API_TOKEN;
        break;
      default:
        authMethod = AuthConstants.AUTH_OAUTH2;
        break;
    }
    String endpoint = args.get(1) != null ? String.valueOf(args.get(1)) : "";
    String insecure = String.valueOf(args.get(2));
    Preferences prefs = Preferences.getInstance();
    prefs.store(Preferences.AUTHENTICATION_METHOD, authMethod);
    prefs.store(Preferences.ENDPOINT_KEY, endpoint);
    prefs.store(Preferences.INSECURE_KEY, insecure);
  }

  private static void registerBridgeFunction(Browser browser) {
    new BrowserFunction(browser, "__ideExecuteCommandBridge__") {
      @Override
      public Object function(Object[] arguments) {
        if (arguments.length < 1 || !(arguments[0] instanceof String)) {
          return null;
        }
        String command = (String) arguments[0];
        String argsJson =
            arguments.length > 1 && arguments[1] instanceof String ? (String) arguments[1] : "[]";
        String callbackId =
            arguments.length > 2 && arguments[2] instanceof String ? (String) arguments[2] : "";

        List<Object> args;
        try {
          args = Arrays.asList(OBJECT_MAPPER.readValue(argsJson, Object[].class));
        } catch (JsonProcessingException e) {
          SnykLogger.logError(e);
          args = Collections.emptyList();
        }

        if ("snyk.login".equals(command) && args.size() >= 3) {
          saveLoginArgs(args);
        }

        final List<Object> finalArgs = args;
        final String finalCallbackId = callbackId;
        CommandHandler.getInstance()
            .executeCommand(command, finalArgs)
            .thenAccept(
                result -> {
                  if (finalCallbackId == null || finalCallbackId.isEmpty()) {
                    return;
                  }
                  try {
                    String resultJson =
                        result == null ? "null" : OBJECT_MAPPER.writeValueAsString(result);
                    String escaped =
                        finalCallbackId.replace("\\", "\\\\").replace("'", "\\'");
                    String script =
                        "if(window.__ideCallbacks__&&window.__ideCallbacks__['"
                            + escaped
                            + "']){"
                            + "var cb=window.__ideCallbacks__['"
                            + escaped
                            + "'];"
                            + "delete window.__ideCallbacks__['"
                            + escaped
                            + "'];"
                            + "cb("
                            + resultJson
                            + ");}";
                    Display.getDefault()
                        .asyncExec(
                            () -> {
                              if (!browser.isDisposed()) {
                                browser.evaluate(script);
                              }
                            });
                  } catch (JsonProcessingException e) {
                    SnykLogger.logError(e);
                  }
                });
        return null;
      }
    };
  }
}
