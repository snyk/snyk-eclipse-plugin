package io.snyk.languageserver;

public interface LsCommandID {
	public static final String SNYK_LOGIN = "snyk.login";
	public static final String SNYK_GET_ACTIVE_USER = "snyk.getActiveUser";
	public static final String SNYK_WORKSPACE_SCAN = "snyk.workspace.scan";
	public static final String SNYK_WORKSPACE_FOLDER_SCAN = "snyk.workspaceFolder.scan";
	public static final String SNYK_TRUST_WORKSPACE_FOLDERS = "snyk.trustWorkspaceFolders";
	public static final String SNYK_SAST_ENABLED = "snyk.getSettingsSastEnabled";
	public static final String SNYK_GENERATE_ISSUE_DESCRIPTION = "snyk.generateIssueDescription";
	public static final String SNYK_REPORT_ANALYTICS = "snyk.reportAnalytics";
}