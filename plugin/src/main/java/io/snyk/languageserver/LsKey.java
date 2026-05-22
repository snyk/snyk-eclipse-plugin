package io.snyk.languageserver;

/** LS protocol setting keys. Each member carries the wire-protocol string sent to the language server. */
public enum LsKey {
    ENDPOINT("api_endpoint"),
    TOKEN("token"),
    ORGANIZATION("organization"),
    ACTIVATE_SNYK_CODE("snyk_code_enabled"),
    ACTIVATE_SNYK_OPEN_SOURCE("snyk_oss_enabled"),
    ACTIVATE_SNYK_IAC("snyk_iac_enabled"),
    INSECURE("proxy_insecure"),
    ADDITIONAL_PARAMS("additional_parameters"),
    SCANNING_MODE("scan_automatic"),
    /** Not in ENTRIES — global scope not yet implemented; folder-scoped env uses LsFolderSettingsKeys.ADDITIONAL_ENV. */
    ADDITIONAL_ENV("additional_environment"),
    SEND_ERROR_REPORTS("send_error_reports"),
    /** Not in ENTRIES — superseded by SEND_ERROR_REPORTS; retained for backward compatibility with stored prefs. */
    ENABLE_TELEMETRY("enableTelemetry"),
    MANAGE_BINARIES_AUTOMATICALLY("automatic_download"),
    CLI_PATH("cli_path"),
    CLI_BASE_DOWNLOAD_URL("binary_base_url"),
    AUTOMATIC_AUTHENTICATION("automatic_authentication"),
    AUTHENTICATION_METHOD("authentication_method"),
    ENABLE_DELTA_FINDINGS("scan_net_new"),
    RISK_SCORE_THRESHOLD("risk_score_threshold"),
    TRUSTED_FOLDERS("trusted_folders"),
    ENABLE_TRUSTED_FOLDERS_FEATURE("trust_enabled"),
    SEVERITY_FILTER_CRITICAL("severity_filter_critical"),
    SEVERITY_FILTER_HIGH("severity_filter_high"),
    SEVERITY_FILTER_MEDIUM("severity_filter_medium"),
    SEVERITY_FILTER_LOW("severity_filter_low"),
    ISSUE_VIEW_OPEN_ISSUES("issue_view_open_issues"),
    ISSUE_VIEW_IGNORED_ISSUES("issue_view_ignored_issues"),
    CLI_RELEASE_CHANNEL("cli_release_channel"),
    /** Not in ENTRIES — sent as a top-level field on LspConfigurationParam, not via the settings map. */
    DEVICE_ID("device_id"),
    ACTIVATE_SNYK_SECRETS("snyk_secrets_enabled");

    public final String key;

    LsKey(String key) {
        this.key = key;
    }
}
