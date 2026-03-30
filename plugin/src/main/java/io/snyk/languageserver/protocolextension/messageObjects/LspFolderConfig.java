package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.google.gson.annotations.SerializedName;

public class LspFolderConfig {

	@SerializedName("folder_path")
	private String folderPath;

	private Map<String, ConfigSetting> settings;

	public String getFolderPath() {
		return folderPath;
	}

	public Map<String, ConfigSetting> getSettings() {
		return settings;
	}

	public LspFolderConfig withSetting(String key, Object value, boolean changed) {
		LspFolderConfig copy = new LspFolderConfig();
		copy.folderPath = this.folderPath;
		copy.settings = new HashMap<>();

		if (this.settings != null) {
			for (Map.Entry<String, ConfigSetting> entry : this.settings.entrySet()) {
				copy.settings.put(entry.getKey(), entry.getValue());
			}
		}

		ConfigSetting existing = (this.settings != null) ? this.settings.get(key) : null;
		ConfigSetting updated = new ConfigSetting();
		updated.setValue(value);
		updated.setChanged(changed);
		if (existing != null) {
			updated.setSource(existing.getSource());
			updated.setOriginScope(existing.getOriginScope());
			updated.setIsLocked(existing.getIsLocked());
		}
		copy.settings.put(key, updated);

		return copy;
	}

	public LspFolderConfig withSettingIfChanged(String key, Object newValue) {
		ConfigSetting existing = (this.settings != null) ? this.settings.get(key) : null;
		if (existing != null && valuesEqual(existing.getValue(), newValue)) {
			return this;
		}
		return withSetting(key, newValue, true);
	}

	private static boolean valuesEqual(Object a, Object b) {
		if (Objects.equals(a, b)) {
			return true;
		}
		if (a == null || b == null) {
			return false;
		}
		if (a instanceof Number && b instanceof Number) {
			return ((Number) a).doubleValue() == ((Number) b).doubleValue();
		}
		return Objects.equals(String.valueOf(a), String.valueOf(b));
	}
}
