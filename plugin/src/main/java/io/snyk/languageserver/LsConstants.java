package io.snyk.languageserver;

public interface LsConstants {
	public static final String COMMAND_LOGIN = "snyk.login";
	public static final String COMMAND_GET_ACTIVE_USER = "snyk.getActiveUser";
	public static final String COMMAND_WORKSPACE_SCAN = "snyk.workspace.scan";
	public static final String COMMAND_WORKSPACE_FOLDER_SCAN = "snyk.workspaceFolder.scan";
	public static final String COMMAND_TRUST_WORKSPACE_FOLDERS = "snyk.trustWorkspaceFolders";
	public static final String COMMAND_GET_SETTINGS_SAST_ENABLED = "snyk.getSettingsSastEnabled";
	public static final String COMMAND_GENERATE_ISSUE_DESCRIPTION = "snyk.generateIssueDescription";
	public static final String COMMAND_REPORT_ANALYTICS = "snyk.reportAnalytics";
	public static final String COMMAND_GET_FEATURE_FLAG_STATUS = "snyk.getFeatureFlagStatus";
	public static final String COMMAND_CODE_FIX_DIFFS = "snyk.code.fixDiffs";
	public static final String COMMAND_CODE_SUBMIT_FIX_FEEDBACK = "snyk.code.submitFixFeedback";
	public static final String SNYK_HAS_AUTHENTICATED = "$/snyk.hasAuthenticated";
	public static final String SNYK_IS_AVAILABLE_CLI = "$/snyk.isAvailableCli";
	public static final String SNYK_ADD_TRUSTED_FOLDERS = "$/snyk.addTrustedFolders";
	public static final String SNYK_SCAN = "$/snyk.scan";
	public static final String SNYK_PUBLISH_DIAGNOSTICS_316 = "$/snyk.publishDiagnostics316";
	public static final String SNYK_FOLDER_CONFIG = "$/snyk.folderConfigs";
}