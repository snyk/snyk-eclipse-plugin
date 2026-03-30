package io.snyk.languageserver;

public final class LsSettingsKeys {

	private LsSettingsKeys() {
		throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
	}

	public static final String ENDPOINT = "endpoint";
	public static final String ORGANIZATION = "organization";
	public static final String TOKEN = "token";
	public static final String ACTIVATE_SNYK_CODE = "activateSnykCode";
	public static final String ACTIVATE_SNYK_OPEN_SOURCE = "activateSnykOpenSource";
	public static final String ACTIVATE_SNYK_IAC = "activateSnykIac";
	public static final String INSECURE = "insecure";
	public static final String ADDITIONAL_PARAMS = "additionalParams";
	public static final String PATH = "path";
	public static final String SCANNING_MODE = "scanningMode";
	public static final String ADDITIONAL_ENV = "additionalEnv";
	public static final String SEND_ERROR_REPORTS = "sendErrorReports";
	public static final String ENABLE_TELEMETRY = "enableTelemetry";
	public static final String MANAGE_BINARIES_AUTOMATICALLY = "manageBinariesAutomatically";
	public static final String CLI_PATH = "cliPath";
	public static final String CLI_BASE_DOWNLOAD_URL = "cliBaseDownloadURL";
	public static final String INTEGRATION_NAME = "integrationName";
	public static final String INTEGRATION_VERSION = "integrationVersion";
	public static final String AUTOMATIC_AUTHENTICATION = "automaticAuthentication";
	public static final String AUTHENTICATION_METHOD = "authenticationMethod";
	public static final String ENABLE_DELTA_FINDINGS = "enableDeltaFindings";
	public static final String RISK_SCORE_THRESHOLD = "riskScoreThreshold";
	public static final String TRUSTED_FOLDERS = "trustedFolders";
	public static final String ENABLE_TRUSTED_FOLDERS_FEATURE = "enableTrustedFoldersFeature";
	public static final String ISSUE_VIEW_OPTIONS = "issueViewOptions";
	public static final String FILTER_SEVERITY = "filterSeverity";
	public static final String REQUIRED_PROTOCOL_VERSION = "requiredProtocolVersion";
	public static final String RUNTIME_NAME = "runtimeName";
	public static final String RUNTIME_VERSION = "runtimeVersion";
	public static final String OS_ARCH = "osArch";
	public static final String OS_PLATFORM = "osPlatform";
}
