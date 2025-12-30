package io.snyk.eclipse.plugin.preferences;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.snyk.eclipse.plugin.html.BaseHtmlProvider;
import io.snyk.eclipse.plugin.properties.FolderConfigs;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;
import io.snyk.languageserver.protocolextension.messageObjects.ScanCommandConfig;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
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

  private static HTMLSettingsPreferencePage instance;
  private Browser browser;
  private final ObjectMapper objectMapper = new ObjectMapper();
  private final BaseHtmlProvider htmlProvider = new BaseHtmlProvider();

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

    browser = new Browser(container, SWT.NONE);
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

    new BrowserFunction(browser, "__ideLogin__") {
      @Override
      public Object function(Object[] arguments) {
        CompletableFuture.runAsync(
            () -> {
              SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
              if (lc == null) {
                SnykLogger.logError(
                    new IllegalStateException("Language client instance is null during login"));
                return;
              }
              lc.triggerAuthentication();
            });
        return null;
      }
    };

    new BrowserFunction(browser, "__ideLogout__") {
      @Override
      public Object function(Object[] arguments) {
        CompletableFuture.runAsync(
            () -> {
              SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
              if (lc == null) {
                SnykLogger.logError(
                    new IllegalStateException("Language client instance is null during logout"));
                return;
              }
              lc.logout();
            });
        return null;
      }
    };
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

      boolean isFallback = Boolean.TRUE.equals(config.getIsFallbackForm());

      // CLI Settings - always persist for both fallback and full forms
      prefs.store(Preferences.CLI_PATH, String.valueOf(config.getCliPath()));
      prefs.store(
          Preferences.MANAGE_BINARIES_AUTOMATICALLY,
          String.valueOf(config.getManageBinariesAutomatically()));
      prefs.store(Preferences.CLI_BASE_URL, String.valueOf(config.getCliBaseDownloadURL()));
      prefs.store(Preferences.RELEASE_CHANNEL, String.valueOf(config.getCliReleaseChannel()));
      prefs.store(Preferences.INSECURE_KEY, String.valueOf(config.getInsecure()));

      // Only persist non-CLI fields if not fallback form
      if (!isFallback) {
        // Scan Settings
        prefs.store(
            Preferences.ACTIVATE_SNYK_OPEN_SOURCE,
            String.valueOf(config.getActivateSnykOpenSource()));
        prefs.store(
            Preferences.ACTIVATE_SNYK_CODE_SECURITY, String.valueOf(config.getActivateSnykCode()));
        prefs.store(Preferences.ACTIVATE_SNYK_IAC, String.valueOf(config.getActivateSnykIac()));

        if (config.getScanningMode() != null) {
          boolean isAutomatic = "auto".equals(config.getScanningMode());
          prefs.store(Preferences.SCANNING_MODE_AUTOMATIC, String.valueOf(isAutomatic));
        }

        // Issue View Settings
        if (config.getIssueViewOptions() != null) {
          IdeConfigData.IssueViewOptions options = config.getIssueViewOptions();
          prefs.store(
              Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, String.valueOf(options.getOpenIssues()));
          prefs.store(
              Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES,
              String.valueOf(options.getIgnoredIssues()));
        }
        prefs.store(Preferences.ENABLE_DELTA, String.valueOf(config.getEnableDeltaFindings()));

        // Authentication Settings
        if (config.getAuthenticationMethod() != null) {
          String method = config.getAuthenticationMethod();
          if (AuthConstants.AUTH_OAUTH2.equals(method)
              || AuthConstants.AUTH_API_TOKEN.equals(method)) {
            prefs.store(Preferences.AUTHENTICATION_METHOD, method);
          }
        }

        // Connection Settings
        prefs.store(Preferences.ENDPOINT_KEY, String.valueOf(config.getEndpoint()));
        prefs.store(Preferences.AUTH_TOKEN_KEY, String.valueOf(config.getToken()));
        prefs.store(Preferences.ORGANIZATION_KEY, String.valueOf(config.getOrganization()));

        // Trusted Folders
        if (config.getTrustedFolders() != null) {
          String trustedFoldersString = String.join(File.pathSeparator, config.getTrustedFolders());
          prefs.store(Preferences.TRUSTED_FOLDERS, trustedFoldersString);
        }

        // Filter Settings
        if (config.getFilterSeverity() != null) {
          IdeConfigData.FilterSeverity severity = config.getFilterSeverity();
          prefs.store(Preferences.FILTER_SHOW_CRITICAL, String.valueOf(severity.getCritical()));
          prefs.store(Preferences.FILTER_SHOW_HIGH, String.valueOf(severity.getHigh()));
          prefs.store(Preferences.FILTER_SHOW_MEDIUM, String.valueOf(severity.getMedium()));
          prefs.store(Preferences.FILTER_SHOW_LOW, String.valueOf(severity.getLow()));
        }
        prefs.store(
            Preferences.RISK_SCORE_THRESHOLD, String.valueOf(config.getRiskScoreThreshold()));

        // Folder Configs
        if (config.getFolderConfigs() != null && !config.getFolderConfigs().isEmpty()) {
          for (IdeConfigData.FolderConfigData folderConfigData : config.getFolderConfigs()) {
            if (folderConfigData.getFolderPath() != null) {
              FolderConfig folderConfig =
                  FolderConfigs.getInstance()
                      .getFolderConfig(Paths.get(folderConfigData.getFolderPath()));

              if (folderConfigData.getPreferredOrg() != null) {
                folderConfig.setPreferredOrg(folderConfigData.getPreferredOrg());
              }
              if (folderConfigData.getAutoDeterminedOrg() != null) {
                folderConfig.setAutoDeterminedOrg(folderConfigData.getAutoDeterminedOrg());
              }
              if (folderConfigData.getOrgSetByUser() != null) {
                folderConfig.setOrgSetByUser(folderConfigData.getOrgSetByUser());
              }
              if (folderConfigData.getAdditionalEnv() != null) {
                folderConfig.setAdditionalEnv(folderConfigData.getAdditionalEnv());
              }
              if (folderConfigData.getAdditionalParameters() != null) {
                String[] params = folderConfigData.getAdditionalParameters().trim().split("\\s+");
                folderConfig.setAdditionalParameters(List.of(params));
              }
              if (folderConfigData.getScanCommandConfig() != null) {
                Map<String, IdeConfigData.ScanCommandConfigData> sourceConfigMap =
                    folderConfigData.getScanCommandConfig();
                Map<String, ScanCommandConfig> targetConfigMap = new HashMap<>();
                for (Map.Entry<String, IdeConfigData.ScanCommandConfigData> entry :
                    sourceConfigMap.entrySet()) {
                  IdeConfigData.ScanCommandConfigData configData = entry.getValue();
                  ScanCommandConfig scanCommandConfig =
                      new ScanCommandConfig(
                          configData.getPreScanCommand(),
                          Boolean.TRUE.equals(configData.getPreScanOnlyReferenceFolder()),
                          configData.getPostScanCommand(),
                          Boolean.TRUE.equals(configData.getPostScanOnlyReferenceFolder()));
                  targetConfigMap.put(entry.getKey(), scanCommandConfig);
                }
                folderConfig.setScanCommandConfig(targetConfigMap);
              }
            }
          }
        }
      }

      // Refresh toolbar UI to reflect changes made in HTML settings
      refreshToolbarUI();
    } catch (JsonProcessingException e) {
      SnykLogger.logError(e);
    }
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
            commandService.refreshElements(
                "io.snyk.eclipse.plugin.commands.snykFilterCritical", null);
            commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykFilterHigh", null);
            commandService.refreshElements(
                "io.snyk.eclipse.plugin.commands.snykFilterMedium", null);
            commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykFilterLow", null);
            // Refresh product filter commands
            commandService.refreshElements("io.snyk.eclipse.plugin.commands.enableOSS", null);
            commandService.refreshElements(
                "io.snyk.eclipse.plugin.commands.enableCodeSecurity", null);
            commandService.refreshElements("io.snyk.eclipse.plugin.commands.enableIAC", null);
            // Refresh issue view options commands
            commandService.refreshElements(
                "io.snyk.eclipse.plugin.commands.snykShowOpenIgnored", null);
            commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykShowIgnored", null);
            // Refresh delta findings command
            commandService.refreshElements(
                "io.snyk.eclipse.plugin.commands.snykFilterNetNewIssues", null);
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
  public void dispose() {
    if (instance == this) {
      instance = null;
    }
    super.dispose();
  }

  public static void notifyAuthTokenChanged(String token) {
    if (instance != null && instance.browser != null && !instance.browser.isDisposed()) {
      Display.getDefault()
          .asyncExec(
              () -> {
                if (instance != null
                    && instance.browser != null
                    && !instance.browser.isDisposed()) {
                  instance.browser.evaluate(
                      "if (typeof window.setAuthToken === 'function') { window.setAuthToken('"
                          + token
                          + "'); }");
                }
              });
    }
  }
}
