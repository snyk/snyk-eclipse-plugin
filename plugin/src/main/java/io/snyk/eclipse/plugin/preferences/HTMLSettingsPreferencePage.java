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
import io.snyk.languageserver.LsKey;
import io.snyk.languageserver.LsSettingsRegistry;
import io.snyk.languageserver.LsSettingsRegistry.Entry;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.ScanCommandConfig;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
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

  // LS keys always saved regardless of fallback/full form (CLI-related fields).
  private static final Set<LsKey> FALLBACK_FORM_KEYS = Set.of(
      LsKey.CLI_PATH,
      LsKey.MANAGE_BINARIES_AUTOMATICALLY,
      LsKey.CLI_BASE_DOWNLOAD_URL,
      LsKey.CLI_RELEASE_CHANNEL,
      LsKey.INSECURE
  );

  private void parseAndSaveConfig(String jsonString) {
    try {
      JsonNode root = objectMapper.readTree(jsonString);
      Preferences prefs = Preferences.getInstance();

      JsonNode fallbackNode = root.get("isFallbackForm");
      boolean isFallback = fallbackNode != null && fallbackNode.booleanValue();

      // Registry-driven loop over all outbound entries.
      for (Entry entry : LsSettingsRegistry.ENTRIES.values()) {
        // Skip hardcoded entries with no pref backing.
        if (entry.prefKey == null) {
          continue;
        }
        // Skip non-fallback fields when processing the fallback form.
        if (isFallback && !FALLBACK_FORM_KEYS.contains(entry.lsKey)) {
          continue;
        }

        // Special case: scan_automatic — form sends "auto"/"manual", pref stores boolean string.
        if (LsKey.SCANNING_MODE == entry.lsKey) {
          JsonNode n = root.get(entry.lsKey.key);
          if (n == null || n.isNull()) {
            prefs.clearExplicitlyChanged(entry.prefKey);
          } else {
            boolean isAutomatic = "auto".equals(n.asText());
            prefs.storeAndTrackChange(entry.prefKey, String.valueOf(isAutomatic));
          }
          continue;
        }

        JsonNode n = root.get(entry.lsKey.key);
        applyFormValue(prefs, entry.prefKey, n);
      }

      // Fields not in ENTRIES — only applied for the full form.
      if (!isFallback) {
        // Severity filters (inbound-only in BY_LS_KEY, not in ENTRIES).
        applyFormValue(prefs, Preferences.FILTER_SHOW_CRITICAL,
            root.get(LsKey.SEVERITY_FILTER_CRITICAL.key));
        applyFormValue(prefs, Preferences.FILTER_SHOW_HIGH,
            root.get(LsKey.SEVERITY_FILTER_HIGH.key));
        applyFormValue(prefs, Preferences.FILTER_SHOW_MEDIUM,
            root.get(LsKey.SEVERITY_FILTER_MEDIUM.key));
        applyFormValue(prefs, Preferences.FILTER_SHOW_LOW,
            root.get(LsKey.SEVERITY_FILTER_LOW.key));

        // risk_score_threshold — integer or null.
        JsonNode riskNode = root.get(LsKey.RISK_SCORE_THRESHOLD.key);
        if (riskNode == null || riskNode.isNull()) {
          prefs.clearExplicitlyChanged(Preferences.RISK_SCORE_THRESHOLD);
        } else {
          prefs.storeAndTrackChange(Preferences.RISK_SCORE_THRESHOLD,
              String.valueOf(riskNode.asInt()));
        }

        // trusted_folders — array of strings joined with File.pathSeparator.
        JsonNode trustedNode = root.get(LsKey.TRUSTED_FOLDERS.key);
        if (trustedNode == null || trustedNode.isNull()) {
          prefs.clearExplicitlyChanged(Preferences.TRUSTED_FOLDERS);
        } else {
          StringBuilder sb = new StringBuilder();
          for (JsonNode element : trustedNode) {
            if (sb.length() > 0) {
              sb.append(File.pathSeparator);
            }
            sb.append(element.asText());
          }
          prefs.storeAndTrackChange(Preferences.TRUSTED_FOLDERS, sb.toString());
        }

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

  private void applyFormValue(Preferences prefs, String key, JsonNode node) {
    if (node == null || node.isNull()) {
      prefs.clearExplicitlyChanged(key);
    } else {
      prefs.storeAndTrackChange(key, nodeToString(node));
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
      JsonNode preferredOrg = folderNode.get(LsFolderSettingsKeys.PREFERRED_ORG);
      if (preferredOrg != null && !preferredOrg.isNull()) {
        config = config.withSettingIfChanged(LsFolderSettingsKeys.PREFERRED_ORG, preferredOrg.asText());
      }
      JsonNode autoDeterminedOrg = folderNode.get(LsFolderSettingsKeys.AUTO_DETERMINED_ORG);
      if (autoDeterminedOrg != null && !autoDeterminedOrg.isNull()) {
        config = config.withSettingIfChanged(LsFolderSettingsKeys.AUTO_DETERMINED_ORG, autoDeterminedOrg.asText());
      }
      JsonNode orgSetByUser = folderNode.get(LsFolderSettingsKeys.ORG_SET_BY_USER);
      if (orgSetByUser != null && !orgSetByUser.isNull()) {
        config = config.withSettingIfChanged(LsFolderSettingsKeys.ORG_SET_BY_USER, orgSetByUser.booleanValue());
      }
      JsonNode additionalEnv = folderNode.get(LsFolderSettingsKeys.ADDITIONAL_ENV);
      if (additionalEnv != null && !additionalEnv.isNull()) {
        config = config.withSettingIfChanged(LsFolderSettingsKeys.ADDITIONAL_ENV, additionalEnv.asText());
      }
      JsonNode additionalParameters = folderNode.get(LsFolderSettingsKeys.ADDITIONAL_PARAMETERS);
      if (additionalParameters != null && !additionalParameters.isNull()) {
        config = config.withSettingIfChanged(
            LsFolderSettingsKeys.ADDITIONAL_PARAMETERS, additionalParameters.asText());
      }
      JsonNode scanCommandConfig = folderNode.get(LsFolderSettingsKeys.SCAN_COMMAND_CONFIG);
      if (scanCommandConfig != null && !scanCommandConfig.isNull()) {
        Map<String, ScanCommandConfig> targetConfigMap = convertScanCommandConfig(scanCommandConfig);
        config = config.withSettingIfChanged(LsFolderSettingsKeys.SCAN_COMMAND_CONFIG, targetConfigMap);
      }
      return config;
    });
  }

  @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
  private Map<String, ScanCommandConfig> convertScanCommandConfig(JsonNode sourceNode) {
    Map<String, ScanCommandConfig> targetConfigMap = new HashMap<>();
    sourceNode.fields().forEachRemaining(entry -> {
      JsonNode configData = entry.getValue();
      ScanCommandConfig scanCommandConfig =
          new ScanCommandConfig(
              configData.has("preScanCommand") && !configData.get("preScanCommand").isNull()
                  ? configData.get("preScanCommand").asText() : null,
              configData.has("preScanOnlyReferenceFolder")
                  && configData.get("preScanOnlyReferenceFolder").booleanValue(),
              configData.has("postScanCommand") && !configData.get("postScanCommand").isNull()
                  ? configData.get("postScanCommand").asText() : null,
              configData.has("postScanOnlyReferenceFolder")
                  && configData.get("postScanOnlyReferenceFolder").booleanValue());
      targetConfigMap.put(entry.getKey(), scanCommandConfig);
    });
    return targetConfigMap;
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
