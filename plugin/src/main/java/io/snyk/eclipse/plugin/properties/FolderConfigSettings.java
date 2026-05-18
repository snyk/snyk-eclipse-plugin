package io.snyk.eclipse.plugin.properties;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsFolderSettingsKeys;
import io.snyk.languageserver.protocolextension.messageObjects.ConfigSetting;
import io.snyk.languageserver.protocolextension.messageObjects.LspFolderConfig;

public class FolderConfigSettings {

	private static FolderConfigSettings instance;

	private final Map<String, LspFolderConfig> configs = new ConcurrentHashMap<>();

	public static synchronized FolderConfigSettings getInstance() {
		if (instance == null) {
			instance = new FolderConfigSettings();
		}
		return instance;
	}

	public static synchronized void setInstance(FolderConfigSettings i) {
		instance = i;
	}

	public void addFolderConfig(LspFolderConfig folderConfig) {
		if (folderConfig.getFolderPath() == null) {
			return;
		}
		String key = normalizePath(folderConfig.getFolderPath());
		configs.put(key, folderConfig);
	}

	/**
	 * Replaces the full folder config state with the incoming snapshot from the LS.
	 * LS protocol v25 guarantees {@code $/snyk.configuration} carries all folders — incoming list
	 * is authoritative; folders absent from it are removed.
	 */
	public synchronized void addAll(List<LspFolderConfig> folderConfigs) {
		// Collect paths present in the incoming list; remove folders no longer reported by LS.
		Set<String> incomingKeys = new HashSet<>();
		for (LspFolderConfig incoming : folderConfigs) {
			if (incoming.getFolderPath() == null) {
				continue;
			}
			String key = normalizePath(incoming.getFolderPath());
			incomingKeys.add(key);
			configs.put(key, incoming);
		}
		configs.keySet().retainAll(incomingKeys);
	}

	public LspFolderConfig getFolderConfig(String folderPath) {
		if (folderPath == null) {
			return createEmptyConfig(null);
		}
		String key = normalizePath(folderPath);
		LspFolderConfig config = configs.get(key);
		if (config == null) {
			return createEmptyConfig(folderPath);
		}
		return config;
	}

	private LspFolderConfig createEmptyConfig(String folderPath) {
		LspFolderConfig config = new LspFolderConfig();
		config.setFolderPath(folderPath);
		config.setSettings(new java.util.HashMap<>());
		return config;
	}

	public synchronized List<LspFolderConfig> getAll() {
		return new ArrayList<>(configs.values());
	}

	public boolean isConfigured(String folderPath) {
		if (folderPath == null) {
			return false;
		}
		String key = normalizePath(folderPath);
		return configs.containsKey(key);
	}

	public void updateFolderConfig(String folderPath, LspFolderConfig config) {
		if (folderPath == null) {
			return;
		}
		String key = normalizePath(folderPath);
		configs.put(key, config);
	}

	public synchronized void computeFolderConfig(String folderPath, Function<LspFolderConfig, LspFolderConfig> updater) {
		if (folderPath == null) {
			return;
		}
		String key = normalizePath(folderPath);
		LspFolderConfig existing = configs.get(key);
		if (existing == null) {
			existing = createEmptyConfig(folderPath);
		}
		configs.put(key, updater.apply(existing));
	}

	public String getBaseBranch(String path) {
		return getStringSettingValue(path, LsFolderSettingsKeys.BASE_BRANCH);
	}

	public String getPreferredOrg(String path) {
		return getStringSettingValue(path, LsFolderSettingsKeys.PREFERRED_ORG);
	}

	public List<String> getLocalBranches(String path) {
		ConfigSetting setting = getSetting(path, LsFolderSettingsKeys.LOCAL_BRANCHES);
		if (setting == null || setting.getValue() == null) {
			return Collections.emptyList();
		}
		Object value = setting.getValue();
		if (value instanceof List<?> rawList) {
			List<String> result = new ArrayList<>();
			for (Object item : rawList) {
				result.add(String.valueOf(item));
			}
			return result;
		}
		return Collections.emptyList();
	}

	public List<String> getAdditionalParameters(String path) {
		ConfigSetting setting = getSetting(path, LsFolderSettingsKeys.ADDITIONAL_PARAMETERS);
		if (setting == null || setting.getValue() == null) {
			return Collections.emptyList();
		}
		Object value = setting.getValue();
		if (value instanceof List<?> rawList) {
			List<String> result = new ArrayList<>();
			for (Object item : rawList) {
				result.add(String.valueOf(item));
			}
			return result;
		}
		if (value instanceof String str && !str.isEmpty()) {
			return List.of(str);
		}
		return Collections.emptyList();
	}

	public String getReferenceFolderPath(String path) {
		return getStringSettingValue(path, LsFolderSettingsKeys.REFERENCE_FOLDER_PATH);
	}

	public boolean isOrgSetByUser(String path) {
		ConfigSetting setting = getSetting(path, LsFolderSettingsKeys.ORG_SET_BY_USER);
		if (setting == null || setting.getValue() == null) {
			return false;
		}
		Object value = setting.getValue();
		if (value instanceof Boolean) {
			return (Boolean) value;
		}
		return Boolean.parseBoolean(String.valueOf(value));
	}

	public String getAutoDeterminedOrg(String path) {
		return getStringSettingValue(path, LsFolderSettingsKeys.AUTO_DETERMINED_ORG);
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
