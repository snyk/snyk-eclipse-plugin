package io.snyk.eclipse.plugin.preferences;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.snyk.eclipse.plugin.html.BaseHtmlProvider;
import io.snyk.eclipse.plugin.html.ExecuteCommandBridge;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsFolderSettingsKeys;
import io.snyk.languageserver.LsKey;
import io.snyk.languageserver.LsSettingsRegistry;
import io.snyk.languageserver.LsSettingsRegistry.Entry;
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.ScanCommandConfig;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class HTMLSettingsPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

  private static final String SELECTED = "selected";

  /**
   * Org-scope folder fields the dialog's "Reset overrides" button clears (sent as JSON null). For
   * these, a null means "remove the user override" → emit {value:null, changed:true} so snyk-ls
   * Unsets the override (fallback to org/LDX/default). preferred_org is included; its LS reset also
   * clears org_set_by_user. Other folder fields (e.g. base_branch) have no fallback, so a null on
   * them stays a no-op. Must stay in sync with the JS FOLDER_RESET_FIELDS list in snyk-ls.
   */
  private static final Set<String> FOLDER_RESET_KEYS = Set.of(
      LsKey.SCANNING_MODE.key,
      LsKey.ENABLE_DELTA_FINDINGS.key,
      LsKey.SEVERITY_FILTER_CRITICAL.key,
      LsKey.SEVERITY_FILTER_HIGH.key,
      LsKey.SEVERITY_FILTER_MEDIUM.key,
      LsKey.SEVERITY_FILTER_LOW.key,
      LsKey.ACTIVATE_SNYK_OPEN_SOURCE.key,
      LsKey.ACTIVATE_SNYK_CODE.key,
      LsKey.ACTIVATE_SNYK_IAC.key,
      LsKey.ACTIVATE_SNYK_SECRETS.key,
      LsKey.ISSUE_VIEW_OPEN_ISSUES.key,
      LsKey.ISSUE_VIEW_IGNORED_ISSUES.key,
      LsKey.RISK_SCORE_THRESHOLD.key,
      LsFolderSettingsKeys.PREFERRED_ORG);

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
    browser.addTraverseListener(e -> {
      if (e.detail == SWT.TRAVERSE_ESCAPE) e.doit = false;
    });
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
          if (parseAndSaveConfig(jsonString)) {
            SnykLanguageServer.promptToRestartEclipseForNewCli();
          }
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
              htmlAttr(prefs.getPref(Preferences.CLI_BASE_URL, Preferences.DEFAULT_CLI_BASE_URL)))
          .replace("{{CLI_PATH}}", htmlAttr(prefs.getCliPath()))
          .replace(
              "{{CHANNEL_STABLE_SELECTED}}",
              Preferences.RELEASE_CHANNEL_STABLE.equals(prefs.getReleaseChannel()) ? SELECTED : "")
          .replace(
              "{{CHANNEL_RC_SELECTED}}",
              Preferences.RELEASE_CHANNEL_RC.equals(prefs.getReleaseChannel()) ? SELECTED : "")
          .replace(
              "{{CHANNEL_PREVIEW_SELECTED}}",
              Preferences.RELEASE_CHANNEL_PREVIEW.equals(prefs.getReleaseChannel()) ? SELECTED : "")
          .replace(
              "{{CHANNEL_CUSTOM_SELECTED}}",
              isCustomChannel(prefs.getReleaseChannel()) ? SELECTED : "")
          .replace(
              "{{CLI_RELEASE_CHANNEL_CUSTOM_HIDDEN}}",
              isCustomChannel(prefs.getReleaseChannel()) ? "" : "hidden")
          .replace(
              "{{CLI_RELEASE_CHANNEL_CUSTOM_VALUE}}",
              isCustomChannel(prefs.getReleaseChannel()) ? htmlAttr(prefs.getReleaseChannel()) : "")
          .replace("{{INSECURE_CHECKED}}", prefs.isInsecure() ? "checked" : "");
    } catch (IOException e) {
      SnykLogger.logError(e);
      return null;
    }
  }

  /**
   * Parses and persists settings JSON from the HTML page. Returns {@code true}
   * if the CLI binary path changed and the caller should prompt the user to
   * restart Eclipse — kept out of this method so the data path stays
   * workbench-free (and unit-testable headless).
   */
  boolean parseAndSaveConfig(String jsonString) {
    try {
      JsonNode root = objectMapper.readTree(jsonString);
      Preferences prefs = Preferences.getInstance();
      String previousCliPath = prefs.getCliPath();

      JsonNode fallbackNode = root.get("isFallbackForm");
      boolean isFallback = fallbackNode != null && fallbackNode.booleanValue();

      for (Entry entry : LsSettingsRegistry.ENTRIES.values()) {
        if (entry.prefKey == null) continue;
        if (isFallback && !entry.useInFallbackForm) continue;

        JsonNode n = root.get(entry.lsKey.key);
        if (n == null) {
          // absent — form didn't send this key, leave tracking untouched
        } else if (n.isNull()) {
          prefs.clearExplicitlyChangedNoFlush(entry.prefKey);
        } else if (entry.formDeserializer != null) {
          prefs.store(entry.prefKey, entry.formDeserializer.apply(n));
          prefs.markExplicitlyChangedNoFlush(entry.prefKey);
        } else {
          prefs.store(entry.prefKey, nodeToString(n));
          prefs.markExplicitlyChangedNoFlush(entry.prefKey);
        }
      }
      prefs.flushExplicitChanges();

      if (!isFallback) {
        // Folder configs.
        JsonNode folderConfigsNode = root.get("folderConfigs");
        if (folderConfigsNode != null && folderConfigsNode.isArray()) {
          for (JsonNode folderNode : folderConfigsNode) {
            processFolderConfig(folderNode);
          }
        }
      }

      // If the user changed the CLI binary path, the running LS process is
      // still bound to the old binary. In-process restart via lsp4e is too
      // unreliable (cached connection providers, stuck failed wrappers,
      // protocol-version probes against the wrong CLI), so signal the caller
      // to prompt for an Eclipse restart — letting normal startup pick up the
      // new binary. We don't invoke the UI prompt here so this method stays
      // headless-safe for unit tests.
      String newCliPath = prefs.getCliPath();
      return !java.util.Objects.equals(previousCliPath, newCliPath);
    } catch (JsonProcessingException e) {
      SnykLogger.logError(e);
      return false;
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
        if ("folderPath".equals(key)) {
          continue;
        }
        if (node.isNull()) {
          // A folder field sent as JSON null is a reset for the org-scope keys: emit
          // {value:null, changed:true} so snyk-ls Unsets the override (fallback to org/LDX/default).
          // Null on any other folder field has no fallback layer, so it stays a no-op.
          if (FOLDER_RESET_KEYS.contains(key)) {
            config = config.withSetting(key, null, true);
          }
          continue;
        }
        if (LsFolderSettingsKeys.SCAN_COMMAND_CONFIG.equals(key)) {
          if (!node.isObject()) {
            SnykLogger.logInfo("Skipping non-object scan_command_config for folder " + pathStr);
            continue;
          }
          try {
            Map<String, ScanCommandConfig> scanCommandMap = objectMapper.convertValue(
                node, objectMapper.getTypeFactory().constructMapType(HashMap.class, String.class, ScanCommandConfig.class));
            config = config.withSetting(key, scanCommandMap, true);
          } catch (IllegalArgumentException e) {
            SnykLogger.logError(e);
            continue;
          }
        } else if (node.isArray()) {
          List<String> list = StreamSupport.stream(node.spliterator(), false)
              .filter(el -> !el.isNull())
              .map(JsonNode::asText)
              .collect(Collectors.toList());
          config = config.withSetting(key, list, true);
        } else if (node.isBoolean()) {
          config = config.withSetting(key, node.booleanValue(), true);
        } else {
          SnykLogger.logInfo("processFolderConfig: storing unknown field type as text for key=" + key
              + " jsonType=" + node.getNodeType());
          config = config.withSetting(key, node.asText(), true);
        }
      }
      return config;
    });
  }

  private static boolean isCustomChannel(String channel) {
    return channel != null
        && !Preferences.RELEASE_CHANNEL_STABLE.equals(channel)
        && !Preferences.RELEASE_CHANNEL_RC.equals(channel)
        && !Preferences.RELEASE_CHANNEL_PREVIEW.equals(channel);
  }

  /** Escapes for double-quoted HTML attribute context only. Do not use in JS/URL contexts. */
  private static String htmlAttr(String v) {
    if (v == null) return "";
    return v.replace("&", "&amp;").replace("\"", "&quot;").replace("<", "&lt;");
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
