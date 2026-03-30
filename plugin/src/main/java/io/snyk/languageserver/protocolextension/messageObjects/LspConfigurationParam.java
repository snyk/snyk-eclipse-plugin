package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.List;
import java.util.Map;

import com.google.gson.annotations.SerializedName;

public class LspConfigurationParam {

	private Map<String, ConfigSetting> settings;

	@SerializedName("folder_configs")
	private List<LspFolderConfig> folderConfigs;

	public LspConfigurationParam() {
	}

	public LspConfigurationParam(Map<String, ConfigSetting> settings, List<LspFolderConfig> folderConfigs) {
		this.settings = settings;
		this.folderConfigs = folderConfigs;
	}

	public Map<String, ConfigSetting> getSettings() {
		return settings;
	}

	public List<LspFolderConfig> getFolderConfigs() {
		return folderConfigs;
	}
}
