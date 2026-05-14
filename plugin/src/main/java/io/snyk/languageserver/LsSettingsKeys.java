package io.snyk.languageserver;

public final class LsSettingsKeys {

	private LsSettingsKeys() {
		throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
	}

	public static final String ENDPOINT = "api_endpoint";
	public static final String ORGANIZATION = "organization";
	public static final String TOKEN = "token";
	public static final String ACTIVATE_SNYK_CODE = "snyk_code_enabled";
	public static final String ACTIVATE_SNYK_OPEN_SOURCE = "snyk_oss_enabled";
	public static final String ACTIVATE_SNYK_IAC = "snyk_iac_enabled";
	public static final String INSECURE = "proxy_insecure";
	public static final String ADDITIONAL_PARAMS = "additional_parameters";
	public static final String SCANNING_MODE = "scan_automatic";
	public static final String ADDITIONAL_ENV = "additional_environment";
	public static final String SEND_ERROR_REPORTS = "send_error_reports";
	public static final String ENABLE_TELEMETRY = "enableTelemetry";
	public static final String MANAGE_BINARIES_AUTOMATICALLY = "automatic_download";
	public static final String CLI_PATH = "cli_path";
	public static final String CLI_BASE_DOWNLOAD_URL = "binary_base_url";
	public static final String AUTOMATIC_AUTHENTICATION = "automatic_authentication";
	public static final String AUTHENTICATION_METHOD = "authentication_method";
	public static final String ENABLE_DELTA_FINDINGS = "scan_net_new";
	public static final String RISK_SCORE_THRESHOLD = "risk_score_threshold";
	public static final String TRUSTED_FOLDERS = "trusted_folders";
	public static final String ENABLE_TRUSTED_FOLDERS_FEATURE = "trust_enabled";
	public static final String SEVERITY_FILTER_CRITICAL = "severity_filter_critical";
	public static final String SEVERITY_FILTER_HIGH = "severity_filter_high";
	public static final String SEVERITY_FILTER_MEDIUM = "severity_filter_medium";
	public static final String SEVERITY_FILTER_LOW = "severity_filter_low";
	public static final String ISSUE_VIEW_OPEN_ISSUES = "issue_view_open_issues";
	public static final String ISSUE_VIEW_IGNORED_ISSUES = "issue_view_ignored_issues";
	public static final String CLI_RELEASE_CHANNEL = "cli_release_channel";
	public static final String DEVICE_ID = "device_id";
	public static final String ACTIVATE_SNYK_SECRETS = "snyk_secrets_enabled";
}
