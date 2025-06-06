package io.snyk.eclipse.plugin.domain;

import java.util.Map;

public final class ProductConstants {

	public static final String SCAN_STATE_IN_PROGRESS = "inProgress";
	public static final String SCAN_STATE_SUCCESS = "success";
	public static final String SCAN_STATE_ERROR = "error";

	public static final String SCAN_PARAMS_OSS = "oss";
	public static final String SCAN_PARAMS_CODE = "code";
	public static final String SCAN_PARAMS_IAC = "iac";

	public static final String DIAGNOSTIC_SOURCE_SNYK_OSS = "Snyk Open Source";
	public static final String DIAGNOSTIC_SOURCE_SNYK_CODE = "Snyk Code";
	public static final String DIAGNOSTIC_SOURCE_SNYK_IAC = "Snyk IaC";

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

	public static final Map<String, String> LSP_SOURCE_TO_SCAN_PARAMS = Map.of(
			DIAGNOSTIC_SOURCE_SNYK_CODE, SCAN_PARAMS_CODE, 
			DIAGNOSTIC_SOURCE_SNYK_IAC, SCAN_PARAMS_IAC, 
			DIAGNOSTIC_SOURCE_SNYK_OSS, SCAN_PARAMS_OSS);

	public static final Map<String, String> SCAN_PARAMS_TO_DISPLAYED = Map.of(
			SCAN_PARAMS_OSS, DISPLAYED_OSS,
			SCAN_PARAMS_IAC, DISPLAYED_IAC,
			SCAN_PARAMS_CODE, DISPLAYED_CODE_SECURITY
	);
}
