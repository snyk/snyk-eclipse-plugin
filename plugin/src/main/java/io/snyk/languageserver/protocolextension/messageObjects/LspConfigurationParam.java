package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.List;
import java.util.Map;

public class LspConfigurationParam {

	private Map<String, ConfigSetting> settings;

	private List<LspFolderConfig> folderConfigs;

	// Metadata fields (init-only, top-level on InitializationOptions)
	private String requiredProtocolVersion;
	private String deviceId;
	private String integrationName;
	private String integrationVersion;
	private String osPlatform;
	private String osArch;
	private String runtimeVersion;
	private String runtimeName;
	private String path;
	private String[] trustedFolders;

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

	public void setRequiredProtocolVersion(String requiredProtocolVersion) {
		this.requiredProtocolVersion = requiredProtocolVersion;
	}

	public void setDeviceId(String deviceId) {
		this.deviceId = deviceId;
	}

	public void setIntegrationName(String integrationName) {
		this.integrationName = integrationName;
	}

	public void setIntegrationVersion(String integrationVersion) {
		this.integrationVersion = integrationVersion;
	}

	public void setOsPlatform(String osPlatform) {
		this.osPlatform = osPlatform;
	}

	public void setOsArch(String osArch) {
		this.osArch = osArch;
	}

	public void setRuntimeVersion(String runtimeVersion) {
		this.runtimeVersion = runtimeVersion;
	}

	public void setRuntimeName(String runtimeName) {
		this.runtimeName = runtimeName;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public void setTrustedFolders(String[] trustedFolders) {
		this.trustedFolders = trustedFolders;
	}

	public String getRequiredProtocolVersion() {
		return requiredProtocolVersion;
	}

	public String getDeviceId() {
		return deviceId;
	}

	public String getIntegrationName() {
		return integrationName;
	}

	public String getIntegrationVersion() {
		return integrationVersion;
	}

	public String getOsPlatform() {
		return osPlatform;
	}

	public String getOsArch() {
		return osArch;
	}

	public String getRuntimeVersion() {
		return runtimeVersion;
	}

	public String getRuntimeName() {
		return runtimeName;
	}

	public String getPath() {
		return path;
	}

	public String[] getTrustedFolders() {
		return trustedFolders;
	}
}
