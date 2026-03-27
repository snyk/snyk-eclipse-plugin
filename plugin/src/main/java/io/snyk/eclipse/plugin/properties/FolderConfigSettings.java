package io.snyk.eclipse.plugin.properties;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsFolderSettingsKeys;
import io.snyk.languageserver.protocolextension.messageObjects.ConfigSetting;
import io.snyk.languageserver.protocolextension.messageObjects.LspFolderConfig;

public class FolderConfigSettings {

	private static FolderConfigSettings instance;

	private final ConcurrentHashMap<String, LspFolderConfig> configs = new ConcurrentHashMap<>();

	public static synchronized FolderConfigSettings getInstance() {
		if (instance == null) {
			instance = new FolderConfigSettings();
		}
		return instance;
	}

	public static void setInstance(FolderConfigSettings i) {
		instance = i;
	}

	public synchronized void addFolderConfig(LspFolderConfig folderConfig) {
		if (folderConfig.getFolderPath() == null) {
			return;
		}
		String key = normalizePath(folderConfig.getFolderPath());
		configs.put(key, folderConfig);
	}

	public synchronized void addAll(List<LspFolderConfig> folderConfigs) {
		configs.clear();
		for (LspFolderConfig config : folderConfigs) {
			try {
				addFolderConfig(config);
			} catch (Exception e) {
				SnykLogger.logError(e);
			}
		}
	}

	public synchronized LspFolderConfig getFolderConfig(String folderPath) {
		if (folderPath == null) {
			return new LspFolderConfig();
		}
		String key = normalizePath(folderPath);
		LspFolderConfig config = configs.get(key);
		if (config == null) {
			return new LspFolderConfig();
		}
		return config;
	}

	public synchronized List<LspFolderConfig> getAll() {
		return new ArrayList<>(configs.values());
	}

	public String getBaseBranch(String path) {
		return getStringSettingValue(path, LsFolderSettingsKeys.BASE_BRANCH);
	}

	public String getPreferredOrg(String path) {
		return getStringSettingValue(path, LsFolderSettingsKeys.PREFERRED_ORG);
	}

	@SuppressWarnings("unchecked")
	public List<String> getLocalBranches(String path) {
		ConfigSetting setting = getSetting(path, LsFolderSettingsKeys.LOCAL_BRANCHES);
		if (setting == null || setting.getValue() == null) {
			return Collections.emptyList();
		}
		Object value = setting.getValue();
		if (value instanceof List<?>) {
			return (List<String>) value;
		}
		return Collections.emptyList();
	}

	public String getAdditionalParameters(String path) {
		return getStringSettingValue(path, LsFolderSettingsKeys.ADDITIONAL_PARAMETERS);
	}

	public String getReferenceFolderPath(String path) {
		return getStringSettingValue(path, LsFolderSettingsKeys.REFERENCE_FOLDER_PATH);
	}

	private ConfigSetting getSetting(String path, String key) {
		LspFolderConfig config = getFolderConfig(path);
		Map<String, ConfigSetting> settings = config.getSettings();
		if (settings == null) {
			return null;
		}
		return settings.get(key);
	}

	private String getStringSettingValue(String path, String key) {
		ConfigSetting setting = getSetting(path, key);
		if (setting == null || setting.getValue() == null) {
			return "";
		}
		return String.valueOf(setting.getValue());
	}

	private String normalizePath(String path) {
		return Paths.get(path).normalize().toAbsolutePath().toString();
	}
}
