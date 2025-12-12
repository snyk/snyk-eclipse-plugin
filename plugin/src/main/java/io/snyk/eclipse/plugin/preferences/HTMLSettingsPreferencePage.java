package io.snyk.eclipse.plugin.preferences;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.browser.ProgressAdapter;
import org.eclipse.swt.browser.ProgressEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.html.BaseHtmlProvider;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class HTMLSettingsPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

	private Browser browser;
	private boolean isUsingFallback;
	private boolean modified;
	private final ObjectMapper objectMapper = new ObjectMapper();
	private final BaseHtmlProvider htmlProvider = new BaseHtmlProvider();

	public HTMLSettingsPreferencePage() {
		super();
		noDefaultAndApplyButton();
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
		initializeBrowserListeners();
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

		new BrowserFunction(browser, "__onFormDirtyChange__") {
			@Override
			public Object function(Object[] arguments) {
				if (arguments.length > 0 && arguments[0] instanceof Boolean) {
					modified = (Boolean) arguments[0];
				} else {
					modified = true;
				}
				return null;
			}
		};

		new BrowserFunction(browser, "__ideLogin__") {
			@Override
			public Object function(Object[] arguments) {
				if (arguments.length > 0 && arguments[0] instanceof String) {
					String jsonConfig = (String) arguments[0];
					if (jsonConfig != null && !jsonConfig.isBlank() && !"login".equals(jsonConfig)) {
						parseAndSaveConfig(jsonConfig);
					}
				}
				CompletableFuture.runAsync(() -> {
					SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
					if (lc != null) {
						lc.updateConfiguration();
						SnykWizard.createAndLaunch();
					}
				});
				return null;
			}
		};

		new BrowserFunction(browser, "__ideLogout__") {
			@Override
			public Object function(Object[] arguments) {
				CompletableFuture.runAsync(() -> {
					SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
					if (lc != null) {
						lc.logout();
					}
				});
				return null;
			}
		};
	}

	private void initializeBrowserListeners() {
		browser.addLocationListener(new LocationListener() {
			@Override
			public void changing(LocationEvent event) {
				String url = event.location;
				if (url.startsWith("http")) {
					event.doit = false;
					Program.launch(url);
				}
			}

			@Override
			public void changed(LocationEvent event) {
			}
		});

		browser.addProgressListener(new ProgressAdapter() {
			@Override
			public void completed(ProgressEvent event) {
				injectCallbackFunctions();
			}
		});
	}

	private void injectCallbackFunctions() {
		String script = """
			(function() {
				if (window.__callbacksInjected__) {
					return;
				}
				window.__callbacksInjected__ = true;
			})();
			""";
		browser.execute(script);
	}

	private void loadContent() {
		// Load fallback HTML first to avoid blocking the UI
		String fallbackHtml = loadFallbackHtml();
		if (fallbackHtml != null && !fallbackHtml.isBlank()) {
			String styledHtml = htmlProvider.replaceCssVariables(fallbackHtml, false);
			browser.setText(styledHtml);
			isUsingFallback = true;
		} else {
			browser.setText("<html><body><p>Loading settings...</p></body></html>");
		}

		// Asynchronously try to get HTML from language server
		CompletableFuture.runAsync(() -> {
			SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
			if (lc != null) {
				String lsHtml = lc.getConfigHtml();
				if (lsHtml != null && !lsHtml.isBlank()) {
					String styledLsHtml = htmlProvider.replaceCssVariables(lsHtml, false);
					Display.getDefault().asyncExec(() -> {
						if (browser != null && !browser.isDisposed()) {
							browser.setText(styledLsHtml);
							isUsingFallback = false;
						}
					});
				}
			}
		}).exceptionally(e -> {
			SnykLogger.logInfo("Could not load HTML from language server: " + e.getMessage());
			return null;
		});
	}

	private String loadFallbackHtml() {
		try (InputStream inputStream = getClass().getResourceAsStream("/ui/html/settings-fallback.html")) {
			if (inputStream == null) {
				SnykLogger.logInfo("Fallback HTML resource not found");
				return null;
			}

			String template = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))
					.lines()
					.collect(Collectors.joining("\n"));

			Preferences prefs = Preferences.getInstance();

			return template
					.replace("{{MANAGE_BINARIES_CHECKED}}", prefs.isManagedBinaries() ? "checked" : "")
					.replace("{{CLI_BASE_DOWNLOAD_URL}}", prefs.getPref(Preferences.CLI_BASE_URL, "https://downloads.snyk.io"))
					.replace("{{CLI_PATH}}", prefs.getCliPath())
					.replace("{{CHANNEL_STABLE_SELECTED}}", "stable".equals(prefs.getReleaseChannel()) ? "selected" : "")
					.replace("{{CHANNEL_RC_SELECTED}}", "rc".equals(prefs.getReleaseChannel()) ? "selected" : "")
					.replace("{{CHANNEL_PREVIEW_SELECTED}}", "preview".equals(prefs.getReleaseChannel()) ? "selected" : "")
					.replace("{{INSECURE_CHECKED}}", prefs.isInsecure() ? "checked" : "");
		} catch (IOException e) {
			SnykLogger.logError(e);
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	private void parseAndSaveConfig(String jsonString) {
		try {
			Map<String, Object> config = objectMapper.readValue(jsonString, new TypeReference<Map<String, Object>>() {});
			Preferences prefs = Preferences.getInstance();

			applyIfPresent(config, "activateSnykOpenSource", value -> 
				prefs.store(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, String.valueOf(value)));
			applyIfPresent(config, "activateSnykCode", value -> 
				prefs.store(Preferences.ACTIVATE_SNYK_CODE_SECURITY, String.valueOf(value)));
			applyIfPresent(config, "activateSnykIac", value -> 
				prefs.store(Preferences.ACTIVATE_SNYK_IAC, String.valueOf(value)));

			applyIfPresent(config, "scanningMode", value -> {
				boolean isAutomatic = "auto".equals(value);
				prefs.store(Preferences.SCANNING_MODE_AUTOMATIC, String.valueOf(isAutomatic));
			});

			applyIfPresent(config, "organization", value -> 
				prefs.store(Preferences.ORGANIZATION_KEY, String.valueOf(value)));
			applyIfPresent(config, "endpoint", value -> 
				prefs.store(Preferences.ENDPOINT_KEY, String.valueOf(value)));
			applyIfPresent(config, "token", value -> 
				prefs.store(Preferences.AUTH_TOKEN_KEY, String.valueOf(value)));
			applyIfPresent(config, "insecure", value -> 
				prefs.store(Preferences.INSECURE_KEY, String.valueOf(value)));

			applyIfPresent(config, "authenticationMethod", value -> {
				String method = String.valueOf(value);
				if (AuthConstants.AUTH_OAUTH2.equals(method)) {
					prefs.store(Preferences.AUTHENTICATION_METHOD, AuthConstants.AUTH_OAUTH2);
				} else if (AuthConstants.AUTH_API_TOKEN.equals(method)) {
					prefs.store(Preferences.AUTHENTICATION_METHOD, AuthConstants.AUTH_API_TOKEN);
				}
			});

			if (config.containsKey("filterSeverity") && config.get("filterSeverity") instanceof Map) {
				Map<String, Object> severity = (Map<String, Object>) config.get("filterSeverity");
				applyIfPresent(severity, "critical", value -> 
					prefs.store(Preferences.FILTER_SHOW_CRITICAL, String.valueOf(value)));
				applyIfPresent(severity, "high", value -> 
					prefs.store(Preferences.FILTER_SHOW_HIGH, String.valueOf(value)));
				applyIfPresent(severity, "medium", value -> 
					prefs.store(Preferences.FILTER_SHOW_MEDIUM, String.valueOf(value)));
				applyIfPresent(severity, "low", value -> 
					prefs.store(Preferences.FILTER_SHOW_LOW, String.valueOf(value)));
			}

			if (config.containsKey("issueViewOptions") && config.get("issueViewOptions") instanceof Map) {
				Map<String, Object> options = (Map<String, Object>) config.get("issueViewOptions");
				applyIfPresent(options, "openIssues", value -> 
					prefs.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, String.valueOf(value)));
				applyIfPresent(options, "ignoredIssues", value -> 
					prefs.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, String.valueOf(value)));
			}

			applyIfPresent(config, "enableDeltaFindings", value -> 
				prefs.store(Preferences.ENABLE_DELTA, String.valueOf(value)));

			applyIfPresent(config, "cliPath", value -> 
				prefs.store(Preferences.CLI_PATH, String.valueOf(value)));
			applyIfPresent(config, "manageBinariesAutomatically", value -> 
				prefs.store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, String.valueOf(value)));
			applyIfPresent(config, "cliBaseDownloadURL", value -> 
				prefs.store(Preferences.CLI_BASE_URL, String.valueOf(value)));
			applyIfPresent(config, "cliReleaseChannel", value -> 
				prefs.store(Preferences.RELEASE_CHANNEL, String.valueOf(value)));

			applyIfPresent(config, "sendErrorReports", value -> 
				prefs.store(Preferences.SEND_ERROR_REPORTS, String.valueOf(value)));
			applyIfPresent(config, "enableTelemetry", value -> 
				prefs.store(Preferences.ENABLE_TELEMETRY, String.valueOf(value)));

			modified = false;

			// Refresh toolbar UI to reflect changes made in HTML settings
			refreshToolbarUI();
		} catch (JsonProcessingException e) {
			SnykLogger.logError(e);
		}
	}

	private void refreshToolbarUI() {
		Display.getDefault().asyncExec(() -> {
			ICommandService commandService = PlatformUI.getWorkbench().getService(ICommandService.class);
			if (commandService != null) {
				// Refresh severity filter commands
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykFilterCritical", null);
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykFilterHigh", null);
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykFilterMedium", null);
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykFilterLow", null);
				// Refresh product filter commands
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.enableOSS", null);
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.enableCodeSecurity", null);
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.enableIAC", null);
				// Refresh issue view options commands
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykShowOpenIgnored", null);
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykShowIgnored", null);
				// Refresh delta findings command
				commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykFilterNetNewIssues", null);
			}
		});
	}

	private void applyIfPresent(Map<String, Object> config, String key, java.util.function.Consumer<Object> consumer) {
		if (config.containsKey(key) && config.get(key) != null) {
			consumer.accept(config.get(key));
		}
	}

	@Override
	public boolean performOk() {
		// Both LS HTML and fallback HTML expose window.getAndSaveIdeConfig()
		browser.evaluate("if (typeof window.getAndSaveIdeConfig === 'function') { window.getAndSaveIdeConfig(); }");

		SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
		if (lc != null) {
			lc.updateConfiguration();
			CompletableFuture.runAsync(() -> {
				lc.refreshFeatureFlags();
			});
		}

		return super.performOk();
	}

	public void refreshWithLsConfig() {
		if (!isUsingFallback) {
			return;
		}

		SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
		if (lc != null) {
			String lsHtml = lc.getConfigHtml();
			if (lsHtml != null && !lsHtml.isBlank()) {
				isUsingFallback = false;
				String styledHtml = htmlProvider.replaceCssVariables(lsHtml, false);
				Display.getDefault().asyncExec(() -> {
					if (browser != null && !browser.isDisposed()) {
						browser.setText(styledHtml);
					}
				});
			}
		}
	}

	public boolean isModified() {
		return modified;
	}
}
