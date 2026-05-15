package io.snyk.languageserver;

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

		for (LsSettingsRegistry.Entry entry : LsSettingsRegistry.ENTRIES.values()) {
			if (entry.prefKey == null) {
				settings.put(entry.lsKey.key, ConfigSetting.outbound(entry.outboundDefault, entry.isAlwaysChanged));
				continue;
			}

			String rawValue;
			if (LsKey.CLI_PATH == entry.lsKey) {
				rawValue = preferences.getCliPath();
			} else {
				rawValue = preferences.getPref(entry.prefKey, entry.outboundDefault);
			}

			Object lsValue = entry.outboundSerializer.apply(rawValue);

			boolean changed = entry.isAlwaysChanged || preferences.isExplicitlyChanged(entry.prefKey);
			for (String additionalKey : entry.additionalChangedPrefKeys) {
				if (preferences.isExplicitlyChanged(additionalKey)) {
					changed = true;
					break;
				}
			}
			settings.put(entry.lsKey.key, ConfigSetting.outbound(lsValue, changed));
		}

		String[] trustedFolders = (String[]) LsSettingsRegistry.ENTRIES.get(LsKey.TRUSTED_FOLDERS)
				.outboundSerializer.apply(preferences.getPref(Preferences.TRUSTED_FOLDERS, ""));

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
		param.setTrustedFolders(trustedFolders);

		return param;
	}

}
