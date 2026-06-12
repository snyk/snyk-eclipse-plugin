package io.snyk.eclipse.plugin.domain;

import java.util.Map;

public final class ProductConstants {

	public static final String SCAN_STATE_IN_PROGRESS = "inProgress";
	public static final String SCAN_STATE_SUCCESS = "success";
	public static final String SCAN_STATE_ERROR = "error";

	public static final String SCAN_PARAMS_OSS = "oss";
	public static final String SCAN_PARAMS_CODE = "code";
	public static final String SCAN_PARAMS_IAC = "iac";
	public static final String SCAN_PARAMS_SECRETS = "secrets";

	public static final String DIAGNOSTIC_SOURCE_SNYK_OSS = "Snyk Open Source";
	public static final String DIAGNOSTIC_SOURCE_SNYK_CODE = "Snyk Code";
	public static final String DIAGNOSTIC_SOURCE_SNYK_IAC = "Snyk IaC";
	public static final String DIAGNOSTIC_SOURCE_SNYK_SECRETS = "Snyk Secrets";

	public static final String DISPLAYED_OSS = "Snyk Open Source";
	public static final String DISPLAYED_CODE_SECURITY = "Code Security";
	public static final String DISPLAYED_IAC = "Configuration";

	public static final String SEVERITY_CRITICAL = "critical";
	public static final String SEVERITY_HIGH = "high";
	public static final String SEVERITY_MEDIUM = "medium";
	public static final String SEVERITY_LOW = "low";

	public static final String FILTERABLE_ISSUE_OPEN_SOURCE = "Open Source";
	public static final String FILTERABLE_ISSUE_CODE_SECURITY = "Code Security";
	public static final String FILTERABLE_ISSUE_INFRASTRUCTURE_AS_CODE = "Infrastructure As Code";

	public static final Map<String, String> FILTERABLE_ISSUE_TYPE_TO_DISPLAY = Map.of(
			FILTERABLE_ISSUE_CODE_SECURITY, DISPLAYED_CODE_SECURITY,
			FILTERABLE_ISSUE_INFRASTRUCTURE_AS_CODE, DISPLAYED_IAC,
			FILTERABLE_ISSUE_OPEN_SOURCE, DISPLAYED_OSS);

	// Accepts both short codenames (navigate_to_range.go) and full product names (SnykMagnetUri/issue_enhancer.go).
	public static final Map<String, String> PRODUCT_TO_DISPLAYED = Map.of(
			"code", DISPLAYED_CODE_SECURITY,
			"Snyk Code", DISPLAYED_CODE_SECURITY,
			"secrets", DISPLAYED_CODE_SECURITY,
			"Snyk Secrets", DISPLAYED_CODE_SECURITY,
			"oss", DISPLAYED_OSS,
			"Snyk Open Source", DISPLAYED_OSS,
			"iac", DISPLAYED_IAC,
			"Snyk IaC", DISPLAYED_IAC
	);
}
