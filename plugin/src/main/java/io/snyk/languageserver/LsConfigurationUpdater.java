package io.snyk.languageserver;

import java.io.File;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.lsp4j.DidChangeConfigurationParams;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.eclipse.plugin.properties.IssueViewOptions;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.ConfigSetting;
import io.snyk.languageserver.protocolextension.messageObjects.FilterSeverity;
import io.snyk.languageserver.protocolextension.messageObjects.LspConfigurationParam;

public class LsConfigurationUpdater {

	public void configurationChanged() {
		SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
		if (lc != null) {
			lc.ensureLanguageServerRunning();
			var languageServer = lc.getConnectedLanguageServer();
			var params = new DidChangeConfigurationParams();
			params.setSettings(buildConfigurationParam());
			languageServer.getWorkspaceService().didChangeConfiguration(params);
		}
	}

	LspConfigurationParam buildConfigurationParam() {
		Preferences preferences = Preferences.getInstance();
		Map<String, ConfigSetting> settings = new LinkedHashMap<>();

		addMachineSetting(settings, preferences, LsSettingsKeys.ACTIVATE_SNYK_OPEN_SOURCE,
				Preferences.ACTIVATE_SNYK_OPEN_SOURCE, Boolean.TRUE.toString());
		addMachineSetting(settings, preferences, LsSettingsKeys.ACTIVATE_SNYK_CODE,
				Preferences.ACTIVATE_SNYK_CODE_SECURITY, Boolean.FALSE.toString());
		addMachineSetting(settings, preferences, LsSettingsKeys.ACTIVATE_SNYK_IAC,
				Preferences.ACTIVATE_SNYK_IAC, Boolean.TRUE.toString());
		addMachineSetting(settings, preferences, LsSettingsKeys.INSECURE,
				Preferences.INSECURE_KEY, Boolean.FALSE.toString());
		addMachineSetting(settings, preferences, LsSettingsKeys.ENDPOINT,
				Preferences.ENDPOINT_KEY, "");
		addMachineSetting(settings, preferences, LsSettingsKeys.ADDITIONAL_PARAMS,
				Preferences.ADDITIONAL_PARAMETERS, "");
		addMachineSetting(settings, preferences, LsSettingsKeys.PATH,
				Preferences.PATH_KEY, "");
		addMachineSetting(settings, preferences, LsSettingsKeys.ORGANIZATION,
				Preferences.ORGANIZATION_KEY, "");

		String scanningMode = preferences.getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC) ? "automatic" : "manual";
		settings.put(LsSettingsKeys.SCANNING_MODE, ConfigSetting.outbound(scanningMode,
				preferences.isExplicitlyChanged(Preferences.SCANNING_MODE_AUTOMATIC)));

		settings.put(LsSettingsKeys.TOKEN, ConfigSetting.outbound(
				preferences.getPref(Preferences.AUTH_TOKEN_KEY, ""),
				preferences.isExplicitlyChanged(Preferences.AUTH_TOKEN_KEY)));

		settings.put(LsSettingsKeys.ADDITIONAL_ENV, ConfigSetting.outbound(
				preferences.getPref(Preferences.ADDITIONAL_ENVIRONMENT, ""),
				preferences.isExplicitlyChanged(Preferences.ADDITIONAL_ENVIRONMENT)));
		settings.put(LsSettingsKeys.SEND_ERROR_REPORTS, ConfigSetting.outbound(
				preferences.getPref(Preferences.SEND_ERROR_REPORTS, ""),
				preferences.isExplicitlyChanged(Preferences.SEND_ERROR_REPORTS)));
		settings.put(LsSettingsKeys.ENABLE_TELEMETRY, ConfigSetting.outbound(
				preferences.getPref(Preferences.ENABLE_TELEMETRY, Boolean.FALSE.toString()),
				preferences.isExplicitlyChanged(Preferences.ENABLE_TELEMETRY)));
		settings.put(LsSettingsKeys.MANAGE_BINARIES_AUTOMATICALLY, ConfigSetting.outbound(
				preferences.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY, Boolean.TRUE.toString()),
				preferences.isExplicitlyChanged(Preferences.MANAGE_BINARIES_AUTOMATICALLY)));
		settings.put(LsSettingsKeys.CLI_PATH, ConfigSetting.outbound(preferences.getCliPath(),
				preferences.isExplicitlyChanged(Preferences.CLI_PATH)));
		settings.put(LsSettingsKeys.CLI_BASE_DOWNLOAD_URL, ConfigSetting.outbound(
				preferences.getPref(Preferences.CLI_BASE_URL, "https://downloads.snyk.io"),
				preferences.isExplicitlyChanged(Preferences.CLI_BASE_URL)));
		settings.put(LsSettingsKeys.INTEGRATION_NAME, ConfigSetting.outbound(Activator.INTEGRATION_NAME, false));
		settings.put(LsSettingsKeys.INTEGRATION_VERSION, ConfigSetting.outbound(Activator.PLUGIN_VERSION, false));
		settings.put(LsSettingsKeys.AUTOMATIC_AUTHENTICATION, ConfigSetting.outbound("false", false));
		settings.put(LsSettingsKeys.AUTHENTICATION_METHOD, ConfigSetting.outbound(
				preferences.getPref(Preferences.AUTHENTICATION_METHOD, AuthConstants.AUTH_OAUTH2),
				preferences.isExplicitlyChanged(Preferences.AUTHENTICATION_METHOD)));
		settings.put(LsSettingsKeys.ENABLE_DELTA_FINDINGS, ConfigSetting.outbound(
				preferences.getPref(Preferences.ENABLE_DELTA, Boolean.FALSE.toString()),
				preferences.isExplicitlyChanged(Preferences.ENABLE_DELTA)));

		Integer riskScoreThreshold = null;
		String riskScoreThresholdStr = preferences.getPref(Preferences.RISK_SCORE_THRESHOLD, "0");
		try {
			int value = Integer.parseInt(riskScoreThresholdStr);
			if (value > 0) {
				riskScoreThreshold = value;
			}
		} catch (NumberFormatException e) { // NOPMD - invalid values default to null (no threshold)
		}
		settings.put(LsSettingsKeys.RISK_SCORE_THRESHOLD, ConfigSetting.outbound(riskScoreThreshold,
				preferences.isExplicitlyChanged(Preferences.RISK_SCORE_THRESHOLD)));

		String trustedFoldersString = preferences.getPref(Preferences.TRUSTED_FOLDERS);
		String[] trustedFolders = new String[0];
		if (trustedFoldersString != null && !trustedFoldersString.isBlank()) {
			trustedFolders = trustedFoldersString.split(File.pathSeparator);
		}
		settings.put(LsSettingsKeys.TRUSTED_FOLDERS, ConfigSetting.outbound(trustedFolders,
				preferences.isExplicitlyChanged(Preferences.TRUSTED_FOLDERS)));
		settings.put(LsSettingsKeys.ENABLE_TRUSTED_FOLDERS_FEATURE, ConfigSetting.outbound(Boolean.TRUE.toString(), false));

		IssueViewOptions issueViewOptions = new IssueViewOptions(
				preferences.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, true),
				preferences.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, false));
		boolean issueViewChanged = preferences.isExplicitlyChanged(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES)
				|| preferences.isExplicitlyChanged(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES);
		settings.put(LsSettingsKeys.ISSUE_VIEW_OPTIONS, ConfigSetting.outbound(issueViewOptions, issueViewChanged));

		FilterSeverity filterSeverity = new FilterSeverity(
				preferences.getBooleanPref(Preferences.FILTER_SHOW_CRITICAL, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_HIGH, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_MEDIUM, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_LOW, true));
		boolean severityChanged = preferences.isExplicitlyChanged(Preferences.FILTER_SHOW_CRITICAL)
				|| preferences.isExplicitlyChanged(Preferences.FILTER_SHOW_HIGH)
				|| preferences.isExplicitlyChanged(Preferences.FILTER_SHOW_MEDIUM)
				|| preferences.isExplicitlyChanged(Preferences.FILTER_SHOW_LOW);
		settings.put(LsSettingsKeys.FILTER_SEVERITY, ConfigSetting.outbound(filterSeverity, severityChanged));

		settings.put(LsSettingsKeys.REQUIRED_PROTOCOL_VERSION, ConfigSetting.outbound(
				io.snyk.languageserver.download.LsBinaries.REQUIRED_LS_PROTOCOL_VERSION, false));

		settings.put(LsSettingsKeys.RUNTIME_NAME, ConfigSetting.outbound(
				org.apache.commons.lang3.SystemUtils.JAVA_RUNTIME_NAME, false));
		settings.put(LsSettingsKeys.RUNTIME_VERSION, ConfigSetting.outbound(
				org.apache.commons.lang3.SystemUtils.JAVA_RUNTIME_VERSION, false));
		settings.put(LsSettingsKeys.OS_ARCH, ConfigSetting.outbound(
				org.apache.commons.lang3.SystemUtils.OS_ARCH, false));
		settings.put(LsSettingsKeys.OS_PLATFORM, ConfigSetting.outbound(
				org.apache.commons.lang3.SystemUtils.OS_NAME, false));

		var folderConfigs = FolderConfigSettings.getInstance().getAll();

		return new LspConfigurationParam(settings, folderConfigs);
	}

	private void addMachineSetting(Map<String, ConfigSetting> settings, Preferences preferences,
			String lsKey, String prefKey, String defaultValue) {
		String value = preferences.getPref(prefKey, defaultValue);
		boolean changed = preferences.isExplicitlyChanged(prefKey);
		settings.put(lsKey, ConfigSetting.outbound(value, changed));
	}

}
