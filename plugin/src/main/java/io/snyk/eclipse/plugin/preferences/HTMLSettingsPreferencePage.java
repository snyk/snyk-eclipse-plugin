package io.snyk.eclipse.plugin.preferences;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.snyk.eclipse.plugin.html.BaseHtmlProvider;
import io.snyk.eclipse.plugin.html.ExecuteCommandBridge;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.snyktoolview.handlers.IHandlerCommands;
import io.snyk.languageserver.LsFolderSettingsKeys;
import io.snyk.languageserver.LsSettingsRegistry;
import io.snyk.languageserver.LsSettingsRegistry.Entry;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.ScanCommandConfig;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

public class HTMLSettingsPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

  private static volatile HTMLSettingsPreferencePage instance;
  private Browser browser;
  private final ObjectMapper objectMapper = new ObjectMapper();
  private final BaseHtmlProvider htmlProvider = new BaseHtmlProvider();

  @SuppressWarnings("PMD.AssignmentToNonFinalStatic")
  public HTMLSettingsPreferencePage() {
    super();
    super.noDefaultAndApplyButton();
    instance = this;
  }

  @Override
  public void init(IWorkbench workbench) {
    setMessage("Snyk Preferences");
  }

  @Override
  protected Control createContents(Composite parent) {
    Composite container = new Composite(parent, SWT.NONE);
    container.setLayout(new FillLayout());

    browser = new Browser(container, SWT.WEBKIT);
    initializeBrowserFunctions();
    loadContent();

    return container;
  }

  private void initializeBrowserFunctions() {
    new BrowserFunction(browser, "__saveIdeConfig__") {
      @Override
      public Object function(Object[] arguments) {
        if (arguments.length > 0 && arguments[0] instanceof String) {
          String jsonString = (String) arguments[0];
          parseAndSaveConfig(jsonString);
        }
        return null;
      }
    };

    ExecuteCommandBridge.install(browser);
  }

  private void loadContent() {
    CompletableFuture.runAsync(
            () -> {
              String lsHtml = null;
              int maxRetries = 2;
              int retryDelayMs = 2000;

              for (int attempt = 1; attempt <= maxRetries; attempt++) {
                SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
                if (lc != null) {
                  lsHtml = lc.getConfigHtml();
                  if (lsHtml != null && !lsHtml.isBlank()) {
                    break;
                  }
                }

                if (attempt < maxRetries) {
                  try {
                    Thread.sleep(retryDelayMs);
                  } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                  }
                }
              }

              String finalHtml;

              if (lsHtml != null && !lsHtml.isBlank()) {
                finalHtml = htmlProvider.replaceCssVariables(lsHtml, false);
              } else {
                SnykLogger.logInfo("Failed to load HTML from language server, using fallback");
                String fallbackHtml = loadFallbackHtml();
                if (fallbackHtml != null && !fallbackHtml.isBlank()) {
                  finalHtml = htmlProvider.replaceCssVariables(fallbackHtml, false);
                } else {
                  SnykLogger.logError(
                      new IllegalStateException("Fallback settings HTML could not be loaded"));
                  return;
                }
              }

              String htmlToDisplay = finalHtml;
              Display.getDefault()
                  .asyncExec(
                      () -> {
                        if (browser == null || browser.isDisposed()) {
                          return;
                        }
                        browser.setText(htmlToDisplay);
                      });
            })
        .exceptionally(
            e -> {
              SnykLogger.logError(new Exception("Failed to load settings HTML", e));
              return null;
            });
  }

  private String loadFallbackHtml() {
    try (InputStream inputStream =
        getClass().getResourceAsStream("/ui/html/settings-fallback.html")) {
      if (inputStream == null) {
        SnykLogger.logError(new IllegalStateException("Fallback HTML resource not found"));
        return null;
      }

      String template = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);

      Preferences prefs = Preferences.getInstance();

      return template
          .replace("{{MANAGE_BINARIES_CHECKED}}", prefs.isManagedBinaries() ? "checked" : "")
          .replace(
              "{{CLI_BASE_DOWNLOAD_URL}}",
              prefs.getPref(Preferences.CLI_BASE_URL, "https://downloads.snyk.io"))
          .replace("{{CLI_PATH}}", prefs.getCliPath())
          .replace(
              "{{CHANNEL_STABLE_SELECTED}}",
              "stable".equals(prefs.getReleaseChannel()) ? "selected" : "")
          .replace(
              "{{CHANNEL_RC_SELECTED}}", "rc".equals(prefs.getReleaseChannel()) ? "selected" : "")
          .replace(
              "{{CHANNEL_PREVIEW_SELECTED}}",
              "preview".equals(prefs.getReleaseChannel()) ? "selected" : "")
          .replace("{{INSECURE_CHECKED}}", prefs.isInsecure() ? "checked" : "");
    } catch (IOException e) {
      SnykLogger.logError(e);
      return null;
    }
  }

  private void parseAndSaveConfig(String jsonString) {
    try {
      JsonNode root = objectMapper.readTree(jsonString);
      Preferences prefs = Preferences.getInstance();

      JsonNode fallbackNode = root.get("isFallbackForm");
      boolean isFallback = fallbackNode != null && fallbackNode.booleanValue();

      for (Entry entry : LsSettingsRegistry.ENTRIES.values()) {
        if (entry.prefKey == null) continue;
        if (isFallback && !entry.useInFallbackForm) continue;

        JsonNode n = root.get(entry.lsKey.key);
        if (n == null) {
          // absent — form didn't send this key, leave tracking untouched
        } else if (n.isNull()) {
          prefs.clearExplicitlyChanged(entry.prefKey);
        } else if (entry.formDeserializer != null) {
          prefs.store(entry.prefKey, entry.formDeserializer.apply(n));
          prefs.markExplicitlyChanged(entry.prefKey);
        } else {
          prefs.store(entry.prefKey, nodeToString(n));
          prefs.markExplicitlyChanged(entry.prefKey);
        }
      }

      if (!isFallback) {
        // Folder configs.
        JsonNode folderConfigsNode = root.get("folderConfigs");
        if (folderConfigsNode != null && folderConfigsNode.isArray()) {
          for (JsonNode folderNode : folderConfigsNode) {
            processFolderConfig(folderNode);
          }
        }
      }

      // Refresh toolbar UI to reflect changes made in HTML settings.
      refreshToolbarUI();
    } catch (JsonProcessingException e) {
      SnykLogger.logError(e);
    }
  }

  private static String nodeToString(JsonNode node) {
    if (node.isBoolean()) {
      return node.asText(); // "true" or "false"
    }
    if (node.isNumber()) {
      double d = node.asDouble();
      if (d == Math.floor(d) && !Double.isInfinite(d)) {
        return String.valueOf((long) d);
      }
      return String.valueOf(d);
    }
    return node.asText();
  }

  private void processFolderConfig(JsonNode folderNode) {
    JsonNode pathNode = folderNode.get("folderPath");
    if (pathNode == null || pathNode.isNull()) {
      return;
    }
    String pathStr = pathNode.asText();
    FolderConfigSettings.getInstance().computeFolderConfig(pathStr, config -> {
      var fields = folderNode.fields();
      while (fields.hasNext()) {
        var field = fields.next();
        String key = field.getKey();
        JsonNode node = field.getValue();
        if ("folderPath".equals(key) || node.isNull()) {
          continue;
        }
        if (LsFolderSettingsKeys.SCAN_COMMAND_CONFIG.equals(key)) {
          Map<String, ScanCommandConfig> scanCommandMap = objectMapper.convertValue(
              node, objectMapper.getTypeFactory().constructMapType(HashMap.class, String.class, ScanCommandConfig.class));
          config = config.withSetting(key, scanCommandMap, true);
        } else if (node.isArray()) {
          List<String> list = new ArrayList<>();
          for (JsonNode el : node) {
            if (!el.isNull()) list.add(el.asText());
          }
          config = config.withSetting(key, list, true);
        } else if (node.isBoolean()) {
          config = config.withSetting(key, node.booleanValue(), true);
        } else {
          config = config.withSetting(key, node.asText(), true);
        }
      }
      return config;
    });
  }

  private void refreshToolbarUI() {
    Display display = Display.getCurrent();
    if (display == null) {
      SnykLogger.logInfo("refreshToolbarUI: No display available, skipping UI refresh");
      return;
    }
    display.asyncExec(
        () -> {
          ICommandService commandService =
              PlatformUI.getWorkbench().getService(ICommandService.class);
          if (commandService != null) {
            // Refresh severity filter commands
            commandService.refreshElements(IHandlerCommands.SHOW_CRITICAL, null);
            commandService.refreshElements(IHandlerCommands.SHOW_HIGH, null);
            commandService.refreshElements(IHandlerCommands.SHOW_MEDIUM, null);
            commandService.refreshElements(IHandlerCommands.SHOW_LOW, null);
            // Refresh product filter commands
            commandService.refreshElements(IHandlerCommands.ENABLE_OSS, null);
            commandService.refreshElements(IHandlerCommands.ENABLE_CODE_SECURITY, null);
            commandService.refreshElements(IHandlerCommands.ENABLE_IAC, null);
            // Refresh issue view options commands
            commandService.refreshElements(IHandlerCommands.IGNORES_SHOW_OPEN, null);
            commandService.refreshElements(IHandlerCommands.IGNORES_SHOW_IGNORED, null);
            // Refresh delta findings command
            commandService.refreshElements(IHandlerCommands.ENABLE_DELTA, null);
          }
        });
  }

  @Override
  public boolean performOk() {
    browser.evaluate(
        "if (typeof window.getAndSaveIdeConfig === 'function') { window.getAndSaveIdeConfig(); }");

    SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
    if (lc != null) {
      CompletableFuture.runAsync(
          () -> {
            lc.updateConfiguration();
            lc.refreshFeatureFlags();
          });
    }

    return super.performOk();
  }

  @Override
  @SuppressWarnings("PMD.NullAssignment")
  public void dispose() {
    if (instance != null && instance.equals(this)) {
      instance = null;
    }
    super.dispose();
  }

  public static void notifyAuthTokenChanged(String token, String apiUrl) {
    if (instance != null && instance.browser != null && !instance.browser.isDisposed()) {
      Display.getDefault()
          .asyncExec(
              () -> {
                if (instance != null
                    && instance.browser != null
                    && !instance.browser.isDisposed()) {
                  String safeToken = ExecuteCommandBridge.escapeForJsString(token);
                  String safeApiUrl = ExecuteCommandBridge.escapeForJsString(apiUrl);
                  instance.browser.evaluate(
                      "if (typeof window.setAuthToken === 'function') { window.setAuthToken('"
                          + safeToken
                          + "', '"
                          + safeApiUrl
                          + "'); }");
                }
              });
    }
  }
}
