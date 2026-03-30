package io.snyk.languageserver;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.lsp4j.DidChangeConfigurationParams;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.eclipse.plugin.properties.FolderConfigs;
import io.snyk.eclipse.plugin.properties.IssueViewOptions;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.ConfigSetting;
import io.snyk.languageserver.protocolextension.messageObjects.FilterSeverity;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;
import io.snyk.languageserver.protocolextension.messageObjects.LspConfigurationParam;
import io.snyk.languageserver.protocolextension.messageObjects.Settings;

public class LsConfigurationUpdater {

	private static final Map<String, String> PREF_TO_LS_KEY = Map.of(
			Preferences.ENDPOINT_KEY, LsSettingsKeys.ENDPOINT,
			Preferences.ORGANIZATION_KEY, LsSettingsKeys.ORGANIZATION,
			Preferences.ACTIVATE_SNYK_CODE_SECURITY, LsSettingsKeys.ACTIVATE_SNYK_CODE,
			Preferences.ACTIVATE_SNYK_OPEN_SOURCE, LsSettingsKeys.ACTIVATE_SNYK_OPEN_SOURCE,
			Preferences.ACTIVATE_SNYK_IAC, LsSettingsKeys.ACTIVATE_SNYK_IAC,
			Preferences.INSECURE_KEY, LsSettingsKeys.INSECURE,
			Preferences.ADDITIONAL_PARAMETERS, LsSettingsKeys.ADDITIONAL_PARAMS,
			Preferences.PATH_KEY, LsSettingsKeys.PATH,
			Preferences.SCANNING_MODE_AUTOMATIC, LsSettingsKeys.SCANNING_MODE);

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
				preferences.getPref(Preferences.AUTH_TOKEN_KEY, ""), false));

		settings.put(LsSettingsKeys.ADDITIONAL_ENV, ConfigSetting.outbound(
				preferences.getPref(Preferences.ADDITIONAL_ENVIRONMENT, ""), false));
		settings.put(LsSettingsKeys.SEND_ERROR_REPORTS, ConfigSetting.outbound(
				preferences.getPref(Preferences.SEND_ERROR_REPORTS, ""), false));
		settings.put(LsSettingsKeys.ENABLE_TELEMETRY, ConfigSetting.outbound(
				preferences.getPref(Preferences.ENABLE_TELEMETRY, Boolean.FALSE.toString()), false));
		settings.put(LsSettingsKeys.MANAGE_BINARIES_AUTOMATICALLY, ConfigSetting.outbound(
				preferences.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY, Boolean.TRUE.toString()), false));
		settings.put(LsSettingsKeys.CLI_PATH, ConfigSetting.outbound(preferences.getCliPath(), false));
		settings.put(LsSettingsKeys.CLI_BASE_DOWNLOAD_URL, ConfigSetting.outbound(
				preferences.getPref(Preferences.CLI_BASE_URL, "https://downloads.snyk.io"), false));
		settings.put(LsSettingsKeys.INTEGRATION_NAME, ConfigSetting.outbound(Activator.INTEGRATION_NAME, false));
		settings.put(LsSettingsKeys.INTEGRATION_VERSION, ConfigSetting.outbound(Activator.PLUGIN_VERSION, false));
		settings.put(LsSettingsKeys.AUTOMATIC_AUTHENTICATION, ConfigSetting.outbound("false", false));
		settings.put(LsSettingsKeys.AUTHENTICATION_METHOD, ConfigSetting.outbound(
				preferences.getPref(Preferences.AUTHENTICATION_METHOD, AuthConstants.AUTH_OAUTH2), false));
		settings.put(LsSettingsKeys.ENABLE_DELTA_FINDINGS, ConfigSetting.outbound(
				preferences.getPref(Preferences.ENABLE_DELTA, Boolean.FALSE.toString()), false));

		Integer riskScoreThreshold = null;
		String riskScoreThresholdStr = preferences.getPref(Preferences.RISK_SCORE_THRESHOLD, "0");
		try {
			int value = Integer.parseInt(riskScoreThresholdStr);
			if (value > 0) {
				riskScoreThreshold = value;
			}
		} catch (NumberFormatException e) { // NOPMD - invalid values default to null (no threshold)
		}
		settings.put(LsSettingsKeys.RISK_SCORE_THRESHOLD, ConfigSetting.outbound(riskScoreThreshold, false));

		String trustedFoldersString = preferences.getPref(Preferences.TRUSTED_FOLDERS);
		String[] trustedFolders = new String[0];
		if (trustedFoldersString != null && !trustedFoldersString.isBlank()) {
			trustedFolders = trustedFoldersString.split(File.pathSeparator);
		}
		settings.put(LsSettingsKeys.TRUSTED_FOLDERS, ConfigSetting.outbound(trustedFolders, false));
		settings.put(LsSettingsKeys.ENABLE_TRUSTED_FOLDERS_FEATURE, ConfigSetting.outbound(Boolean.TRUE.toString(), false));

		IssueViewOptions issueViewOptions = new IssueViewOptions(
				preferences.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, true),
				preferences.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, false));
		settings.put(LsSettingsKeys.ISSUE_VIEW_OPTIONS, ConfigSetting.outbound(issueViewOptions, false));

		FilterSeverity filterSeverity = new FilterSeverity(
				preferences.getBooleanPref(Preferences.FILTER_SHOW_CRITICAL, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_HIGH, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_MEDIUM, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_LOW, true));
		settings.put(LsSettingsKeys.FILTER_SEVERITY, ConfigSetting.outbound(filterSeverity, false));

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

	Settings getCurrentSettings() {
		Preferences preferences = Preferences.getInstance();
		String activateSnykOpenSource = preferences.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE,
				Boolean.TRUE.toString());
		String activateSnykCodeSecurity = preferences.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY,
				Boolean.FALSE.toString());
		String activateSnykIac = preferences.getPref(Preferences.ACTIVATE_SNYK_IAC, Boolean.TRUE.toString());
		String insecure = preferences.getPref(Preferences.INSECURE_KEY, Boolean.FALSE.toString());
		String endpoint = preferences.getPref(Preferences.ENDPOINT_KEY, "");
		String additionalParams = preferences.getPref(Preferences.ADDITIONAL_PARAMETERS, "");
		String additionalEnv = preferences.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "");
		String path = preferences.getPref(Preferences.PATH_KEY, "");
		IssueViewOptions issueViewOptions = new IssueViewOptions(
				preferences.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, true),
				preferences.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, false));
		String sendErrorReports = preferences.getPref(Preferences.SEND_ERROR_REPORTS, "");
		String enableTelemetry = preferences.getPref(Preferences.ENABLE_TELEMETRY, Boolean.FALSE.toString());
		String organization = preferences.getPref(Preferences.ORGANIZATION_KEY, "");
		String manageBinariesAutomatically = preferences.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY,
				Boolean.TRUE.toString());
		String cliPath = preferences.getCliPath();
		String cliBaseDownloadURL = preferences.getPref(Preferences.CLI_BASE_URL, "https://downloads.snyk.io");
		String token = preferences.getPref(Preferences.AUTH_TOKEN_KEY, "");
		String integrationName = Activator.INTEGRATION_NAME;
		String integrationVersion = Activator.PLUGIN_VERSION;
		String automaticAuthentication = "false";
		String trustedFoldersString = preferences.getPref(Preferences.TRUSTED_FOLDERS);
		String[] trustedFolders = new String[0];
		if (trustedFoldersString != null && !trustedFoldersString.isBlank()) {
			trustedFolders = trustedFoldersString.split(File.pathSeparator);
		}
		String enableTrustedFolderFeature = Boolean.TRUE.toString();
		String scanningMode = preferences.getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC) ? "automatic" : "manual";
		String authenticationMethod = preferences.getPref(Preferences.AUTHENTICATION_METHOD, AuthConstants.AUTH_OAUTH2);

		String enableDeltaFindings = preferences.getPref(Preferences.ENABLE_DELTA, Boolean.FALSE.toString());

		Integer riskScoreThreshold = null;
		String riskScoreThresholdStr = preferences.getPref(Preferences.RISK_SCORE_THRESHOLD, "0");
		try {
			int value = Integer.parseInt(riskScoreThresholdStr);
			if (value > 0) {
				riskScoreThreshold = value;
			}
		} catch (NumberFormatException e) { // NOPMD - invalid values default to null (no threshold)
		}

		// only add folder configs that are initialized
		var folderConfigs = new ArrayList<FolderConfig>();
		for (var p : Collections.unmodifiableSet(FolderConfigs.LanguageServerConfigReceived)) {
			folderConfigs.add(FolderConfigs.getInstance().getFolderConfig(p));
		}

		FilterSeverity filterSeverity = new FilterSeverity(
				preferences.getBooleanPref(Preferences.FILTER_SHOW_CRITICAL, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_HIGH, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_MEDIUM, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_LOW, true));

		return new Settings(activateSnykOpenSource, activateSnykCodeSecurity, activateSnykIac,
				insecure, endpoint, additionalParams, additionalEnv, path, issueViewOptions, sendErrorReports,
				enableTelemetry, organization, manageBinariesAutomatically, cliPath, cliBaseDownloadURL, token,
				integrationName, integrationVersion, automaticAuthentication, trustedFolders, enableTrustedFolderFeature,
				scanningMode, enableDeltaFindings, riskScoreThreshold, authenticationMethod, folderConfigs, filterSeverity);
	}
}
