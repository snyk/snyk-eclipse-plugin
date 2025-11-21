package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.List;

import org.apache.commons.lang3.SystemUtils;

import io.snyk.eclipse.plugin.properties.IssueViewOptions;
import io.snyk.languageserver.download.LsBinaries;

public class Settings {
	private final String activateSnykOpenSource;
	private final String activateSnykCodeSecurity;
	private final String activateSnykIac;

	private final String insecure;
	private final String endpoint;
	private final String additionalParams;
	private final String additionalEnv;
	private final String path;
	private final IssueViewOptions issueViewOptions;
	private final String sendErrorReports;
	private final String enableTelemetry;
	private final String organization;
	private final String manageBinariesAutomatically;
	private final String cliPath;
	private final String token;
	private final String integrationName;
	private final String integrationVersion;
	private final String automaticAuthentication;
	private final String[] trustedFolders;
	private final String enableTrustedFoldersFeature;
	private final String runtimeName = SystemUtils.JAVA_RUNTIME_NAME;
	private final String runtimeVersion = SystemUtils.JAVA_RUNTIME_VERSION;
	private final String osArch = SystemUtils.OS_ARCH;
	private final String osPlatform = SystemUtils.OS_NAME;
	private final String scanningMode;
	private final String enableDeltaFindings;
	private final String requiredProtocolVersion = LsBinaries.REQUIRED_LS_PROTOCOL_VERSION;
	private final String authenticationMethod;
	private final List<FolderConfig> folderConfigs;

	public Settings(String activateSnykOpenSource, String activateSnykCodeSecurity, String activateSnykIac,
			String insecure, String endpoint, String additionalParams, String additionalEnv, String path,
			IssueViewOptions issueViewOptions, String sendErrorReports, String enableTelemetry, String organization,
			String manageBinariesAutomatically, String cliPath, String token, String integrationName,
			String integrationVersion, String automaticAuthentication, String[] trustedFolders,
			String enableTrustedFoldersFeature, String scanningMode, String enableDeltaFindings,
			String authenticationMethod, List<FolderConfig> folderConfigs) {
		this.activateSnykOpenSource = activateSnykOpenSource;
		this.activateSnykCodeSecurity = activateSnykCodeSecurity;
		this.activateSnykIac = activateSnykIac;
		this.insecure = insecure;
		this.endpoint = endpoint;
		this.additionalParams = additionalParams;
		this.additionalEnv = additionalEnv;
		this.path = path;
		this.issueViewOptions = issueViewOptions;
		this.sendErrorReports = sendErrorReports;
		this.enableTelemetry = enableTelemetry;
		this.organization = organization;
		this.manageBinariesAutomatically = manageBinariesAutomatically;
		this.cliPath = cliPath;
		this.token = token;
		this.integrationName = integrationName;
		this.integrationVersion = integrationVersion;
		this.automaticAuthentication = automaticAuthentication;
		this.trustedFolders = trustedFolders.clone();
		this.enableTrustedFoldersFeature = enableTrustedFoldersFeature;
		this.scanningMode = scanningMode;
		this.enableDeltaFindings = enableDeltaFindings;
		this.authenticationMethod = authenticationMethod;
		this.folderConfigs = folderConfigs;
	}

	public String getPath() {
		return path;
	}

	public String getActivateSnykOpenSource() {
		return activateSnykOpenSource;
	}

	public String getActivateSnykCodeSecurity() {
		return activateSnykCodeSecurity;
	}

	public String getActivateSnykIac() {
		return activateSnykIac;
	}

	public String getInsecure() {
		return insecure;
	}

	public String getEndpoint() {
		return endpoint;
	}

	public String getAdditionalParams() {
		return additionalParams;
	}

	public String getAdditionalEnv() {
		return additionalEnv;
	}

	public IssueViewOptions getIssueViewOptions() {
		return this.issueViewOptions;
	}

	public String getSendErrorReports() {
		return this.sendErrorReports;
	}

	public String getEnableTelemetry() {
		return this.enableTelemetry;
	}

	public String getOrganization() {
		return this.organization;
	}

	public String getManageBinariesAutomatically() {
		return this.manageBinariesAutomatically;
	}

	public String getCliPath() {
		return cliPath;
	}

	public String getToken() {
		return token;
	}

	public String getIntegrationName() {
		return integrationName;
	}

	public String getIntegrationVersion() {
		return integrationVersion;
	}

	public String getAutomaticAuthentication() {
		return automaticAuthentication;
	}

	public String[] getTrustedFolders() {
		return trustedFolders.clone();
	}

	public String getEnableTrustedFoldersFeature() {
		return enableTrustedFoldersFeature;
	}

	public String getRuntimeName() {
		return runtimeName;
	}

	public String getRuntimeVersion() {
		return runtimeVersion;
	}

	public String getOsArch() {
		return osArch;
	}

	public String getOsPlatform() {
		return osPlatform;
	}

	public String getScanningMode() {
		return scanningMode;
	}

	public String getAuthenticationMethod() {
		return authenticationMethod;
	}

	public String getRequiredProtocolVersion() {
		return requiredProtocolVersion;
	}

	public List<FolderConfig> getFolderConfigs() {
		return folderConfigs;
	}

	public String getEnableDeltaFindings() {
		return enableDeltaFindings;
	}
}
