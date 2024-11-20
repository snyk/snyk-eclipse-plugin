package io.snyk.eclipse.plugin.domain;

import java.util.Map;

public interface ProductConstants {
	String SCAN_STATE_IN_PROGRESS="inProgress";
	String SCAN_STATE_SUCCESS="success";
	String SCAN_STATE_ERROR="error";
		
	String SCAN_PARAMS_OSS = "oss";
	String SCAN_PARAMS_CODE = "code";
	String SCAN_PARAMS_IAC = "iac";

	String DIAGNOSTIC_SOURCE_SNYK_OSS = "Snyk Open Source";
	String DIAGNOSTIC_SOURCE_SNYK_CODE = "Snyk Code";
	String DIAGNOSTIC_SOURCE_SNYK_IAC = "Snyk IaC";

	String DISPLAYED_OSS = "Snyk Open Source";
	String DISPLAYED_CODE_SECURITY = "Code Security";
	String DISPLAYED_CODE_QUALITY = "Code Quality";
	String DISPLAYED_IAC = "Configuration";

	String SEVERITY_CRITICAL = "critical";
	String SEVERITY_HIGH = "high";
	String SEVERITY_MEDIUM = "medium";
	String SEVERITY_LOW = "low";
	
	Map<String, String> LSP_SOURCE_TO_SCAN_PARAMS = Map.of(DIAGNOSTIC_SOURCE_SNYK_CODE, SCAN_PARAMS_CODE,
			DIAGNOSTIC_SOURCE_SNYK_IAC, SCAN_PARAMS_IAC, DIAGNOSTIC_SOURCE_SNYK_OSS, SCAN_PARAMS_OSS);

	// code cannot be mapped easily
	Map<String, String> SCAN_PARAMS_TO_DISPLAYED = Map.of(SCAN_PARAMS_OSS, DISPLAYED_OSS, SCAN_PARAMS_IAC, DISPLAYED_IAC);
}
