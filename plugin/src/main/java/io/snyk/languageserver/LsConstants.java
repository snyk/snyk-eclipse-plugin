package io.snyk.languageserver;

public interface LsConstants {
	String COMMAND_LOGIN = "snyk.login";
	String COMMAND_GET_ACTIVE_USER = "snyk.getActiveUser";
	String COMMAND_WORKSPACE_SCAN = "snyk.workspace.scan";
	String COMMAND_WORKSPACE_FOLDER_SCAN = "snyk.workspaceFolder.scan";
	String COMMAND_TRUST_WORKSPACE_FOLDERS = "snyk.trustWorkspaceFolders";
	String COMMAND_GET_SETTINGS_SAST_ENABLED = "snyk.getSettingsSastEnabled";
	String COMMAND_GENERATE_ISSUE_DESCRIPTION = "snyk.generateIssueDescription";
	String COMMAND_REPORT_ANALYTICS = "snyk.reportAnalytics";
	String COMMAND_GET_FEATURE_FLAG_STATUS = "snyk.getFeatureFlagStatus";
	String COMMAND_CODE_FIX_DIFFS = "snyk.code.fixDiffs";
	String COMMAND_CODE_SUBMIT_FIX_FEEDBACK = "snyk.code.submitFixFeedback";
	String COMMAND_SNYK_CLI = "snyk.executeCLI";
	String SNYK_HAS_AUTHENTICATED = "$/snyk.hasAuthenticated";
	String SNYK_IS_AVAILABLE_CLI = "$/snyk.isAvailableCli";
	String SNYK_ADD_TRUSTED_FOLDERS = "$/snyk.addTrustedFolders";
	String SNYK_SCAN = "$/snyk.scan";
	String SNYK_PUBLISH_DIAGNOSTICS_316 = "$/snyk.publishDiagnostics316";
	String SNYK_FOLDER_CONFIG = "$/snyk.folderConfigs";
	String SNYK_SCAN_SUMMARY = "$/snyk.scanSummary";
}