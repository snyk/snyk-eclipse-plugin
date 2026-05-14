package io.snyk.languageserver;

import java.io.File;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.lsp4j.DidChangeConfigurationParams;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.ConfigSetting;
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

		for (LsSettingsRegistry.Entry entry : LsSettingsRegistry.ENTRIES) {
			if (entry.alwaysFixed) {
				settings.put(entry.lsKey, ConfigSetting.outbound(entry.outboundDefault, false));
				continue;
			}

			String rawValue;
			if (LsSettingsKeys.CLI_PATH.equals(entry.lsKey)) {
				rawValue = preferences.getCliPath();
			} else {
				rawValue = preferences.getPref(entry.prefKey, entry.outboundDefault);
			}

			Object lsValue = entry.outboundSerializer.apply(rawValue);
			boolean changed = preferences.isExplicitlyChanged(entry.prefKey);
			settings.put(entry.lsKey, ConfigSetting.outbound(lsValue, changed));
		}

		// risk_score_threshold — int or null (null = no threshold)
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

		// Severity filters — 4 individual boolean keys (LS has no composite "enabled_severities" key)
		boolean anySeverityChanged = preferences.isExplicitlyChanged(Preferences.FILTER_SHOW_CRITICAL)
				|| preferences.isExplicitlyChanged(Preferences.FILTER_SHOW_HIGH)
				|| preferences.isExplicitlyChanged(Preferences.FILTER_SHOW_MEDIUM)
				|| preferences.isExplicitlyChanged(Preferences.FILTER_SHOW_LOW);
		settings.put(LsSettingsKeys.SEVERITY_FILTER_CRITICAL, ConfigSetting.outbound(
				preferences.getBooleanPref(Preferences.FILTER_SHOW_CRITICAL, true), anySeverityChanged));
		settings.put(LsSettingsKeys.SEVERITY_FILTER_HIGH, ConfigSetting.outbound(
				preferences.getBooleanPref(Preferences.FILTER_SHOW_HIGH, true), anySeverityChanged));
		settings.put(LsSettingsKeys.SEVERITY_FILTER_MEDIUM, ConfigSetting.outbound(
				preferences.getBooleanPref(Preferences.FILTER_SHOW_MEDIUM, true), anySeverityChanged));
		settings.put(LsSettingsKeys.SEVERITY_FILTER_LOW, ConfigSetting.outbound(
				preferences.getBooleanPref(Preferences.FILTER_SHOW_LOW, true), anySeverityChanged));

		// trusted_folders in settings map (for didChangeConfiguration; also set top-level for InitializationOptions)
		String trustedFoldersString = preferences.getPref(Preferences.TRUSTED_FOLDERS);
		String[] trustedFolders = new String[0];
		if (trustedFoldersString != null && !trustedFoldersString.isBlank()) {
			trustedFolders = trustedFoldersString.split(File.pathSeparator);
		}
		settings.put(LsSettingsKeys.TRUSTED_FOLDERS, ConfigSetting.outbound(trustedFolders, true));

		var folderConfigs = FolderConfigSettings.getInstance().getAll();

		var param = new LspConfigurationParam(settings, folderConfigs);

		param.setRequiredProtocolVersion(
				io.snyk.languageserver.download.LsBinaries.REQUIRED_LS_PROTOCOL_VERSION);
		param.setIntegrationName(Activator.INTEGRATION_NAME);
		param.setIntegrationVersion(Activator.PLUGIN_VERSION);
		param.setRuntimeName(org.apache.commons.lang3.SystemUtils.JAVA_RUNTIME_NAME);
		param.setRuntimeVersion(org.apache.commons.lang3.SystemUtils.JAVA_RUNTIME_VERSION);
		param.setOsArch(org.apache.commons.lang3.SystemUtils.OS_ARCH);
		param.setOsPlatform(org.apache.commons.lang3.SystemUtils.OS_NAME);
		param.setPath(preferences.getPref(Preferences.PATH_KEY, ""));
		param.setDeviceId(preferences.getPref(Preferences.DEVICE_ID, ""));
		// also set top-level for InitializationOptions path
		param.setTrustedFolders(trustedFolders);

		return param;
	}

}
