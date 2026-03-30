package io.snyk.eclipse.plugin.preferences;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.snyk.eclipse.plugin.html.BaseHtmlProvider;
import io.snyk.eclipse.plugin.html.ExecuteCommandBridge;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.snyktoolview.handlers.IHandlerCommands;
import io.snyk.languageserver.LsFolderSettingsKeys;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.ScanCommandConfig;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
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
      IdeConfigData config = objectMapper.readValue(jsonString, IdeConfigData.class);
      Preferences prefs = Preferences.getInstance();

      boolean isFallback = Boolean.TRUE.equals(config.isFallbackForm());

      // CLI Settings - always persist for both fallback and full forms
      applyFormValue(prefs, Preferences.CLI_PATH, config.cliPath());
      applyFormValue(prefs, Preferences.MANAGE_BINARIES_AUTOMATICALLY, config.manageBinariesAutomatically());
      applyFormValue(prefs, Preferences.CLI_BASE_URL, config.cliBaseDownloadURL());
      applyFormValue(prefs, Preferences.RELEASE_CHANNEL, config.cliReleaseChannel());
      applyFormValue(prefs, Preferences.INSECURE_KEY, config.insecure());

      // Only persist non-CLI fields if not fallback form
      if (!isFallback) {
        // Scan Settings
        applyFormValue(prefs, Preferences.ACTIVATE_SNYK_OPEN_SOURCE, config.activateSnykOpenSource());
        applyFormValue(prefs, Preferences.ACTIVATE_SNYK_CODE_SECURITY, config.activateSnykCode());
        applyFormValue(prefs, Preferences.ACTIVATE_SNYK_IAC, config.activateSnykIac());

        if (config.scanningMode() != null) {
          boolean isAutomatic = "auto".equals(config.scanningMode());
          prefs.storeAndTrackChange(Preferences.SCANNING_MODE_AUTOMATIC, String.valueOf(isAutomatic));
        } else {
          prefs.clearExplicitlyChanged(Preferences.SCANNING_MODE_AUTOMATIC);
        }

        // Issue View Settings
        if (config.issueViewOptions() != null) {
          IdeConfigData.IssueViewOptions options = config.issueViewOptions();
          applyFormValue(prefs, Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, options.openIssues());
          applyFormValue(prefs, Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, options.ignoredIssues());
        }
        applyFormValue(prefs, Preferences.ENABLE_DELTA, config.enableDeltaFindings());

        // Authentication Settings
        applyFormValue(prefs, Preferences.AUTHENTICATION_METHOD, config.authenticationMethod());

        // Connection Settings
        applyFormValue(prefs, Preferences.ENDPOINT_KEY, config.endpoint());
        applyFormValue(prefs, Preferences.AUTH_TOKEN_KEY, config.token());
        applyFormValue(prefs, Preferences.ORGANIZATION_KEY, config.organization());

        // Trusted Folders
        if (config.trustedFolders() != null) {
          String trustedFoldersString = String.join(File.pathSeparator, config.trustedFolders());
          prefs.storeAndTrackChange(Preferences.TRUSTED_FOLDERS, trustedFoldersString);
        } else {
          prefs.clearExplicitlyChanged(Preferences.TRUSTED_FOLDERS);
        }

        // Filter Settings
        if (config.filterSeverity() != null) {
          IdeConfigData.FilterSeverity severity = config.filterSeverity();
          applyFormValue(prefs, Preferences.FILTER_SHOW_CRITICAL, severity.critical());
          applyFormValue(prefs, Preferences.FILTER_SHOW_HIGH, severity.high());
          applyFormValue(prefs, Preferences.FILTER_SHOW_MEDIUM, severity.medium());
          applyFormValue(prefs, Preferences.FILTER_SHOW_LOW, severity.low());
        }
        applyFormValue(prefs, Preferences.RISK_SCORE_THRESHOLD, config.riskScoreThreshold());

        // Folder Configs
        if (config.folderConfigs() != null && !config.folderConfigs().isEmpty()) {
          for (IdeConfigData.FolderConfigData folderConfigData : config.folderConfigs()) {
            processFolderConfig(folderConfigData);
          }
        }
      }

      // Refresh toolbar UI to reflect changes made in HTML settings
      refreshToolbarUI();
    } catch (JsonProcessingException e) {
      SnykLogger.logError(e);
    }
  }

  private void applyFormValue(Preferences prefs, String key, Object value) {
    if (value == null) {
      prefs.clearExplicitlyChanged(key);
    } else {
      prefs.storeAndTrackChange(key, String.valueOf(value));
    }
  }

  private void processFolderConfig(IdeConfigData.FolderConfigData folderConfigData) {
    if (folderConfigData.folderPath() == null) {
      return;
    }

    String pathStr = folderConfigData.folderPath();
    FolderConfigSettings.getInstance().computeFolderConfig(pathStr, config -> {
      if (folderConfigData.preferredOrg() != null) {
        config = config.withSettingIfChanged(LsFolderSettingsKeys.PREFERRED_ORG, folderConfigData.preferredOrg());
      }
      if (folderConfigData.autoDeterminedOrg() != null) {
        config = config.withSettingIfChanged(LsFolderSettingsKeys.AUTO_DETERMINED_ORG, folderConfigData.autoDeterminedOrg());
      }
      if (folderConfigData.orgSetByUser() != null) {
        config = config.withSettingIfChanged(LsFolderSettingsKeys.ORG_SET_BY_USER, folderConfigData.orgSetByUser());
      }
      if (folderConfigData.additionalEnv() != null) {
        config = config.withSettingIfChanged(LsFolderSettingsKeys.ADDITIONAL_ENV, folderConfigData.additionalEnv());
      }
      if (folderConfigData.additionalParameters() != null) {
        config = config.withSettingIfChanged(LsFolderSettingsKeys.ADDITIONAL_PARAMETERS, folderConfigData.additionalParameters());
      }
      if (folderConfigData.scanCommandConfig() != null) {
        Map<String, ScanCommandConfig> targetConfigMap =
            convertScanCommandConfig(folderConfigData.scanCommandConfig());
        config = config.withSettingIfChanged(LsFolderSettingsKeys.SCAN_COMMAND_CONFIG, targetConfigMap);
      }
      return config;
    });
  }

  @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
  private Map<String, ScanCommandConfig> convertScanCommandConfig(
      Map<String, IdeConfigData.ScanCommandConfigData> sourceConfigMap) {
    Map<String, ScanCommandConfig> targetConfigMap = new HashMap<>();
    for (Map.Entry<String, IdeConfigData.ScanCommandConfigData> entry :
        sourceConfigMap.entrySet()) {
      IdeConfigData.ScanCommandConfigData configData = entry.getValue();
      ScanCommandConfig scanCommandConfig =
          new ScanCommandConfig(
              configData.preScanCommand(),
              Boolean.TRUE.equals(configData.preScanOnlyReferenceFolder()),
              configData.postScanCommand(),
              Boolean.TRUE.equals(configData.postScanOnlyReferenceFolder()));
      targetConfigMap.put(entry.getKey(), scanCommandConfig);
    }
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
