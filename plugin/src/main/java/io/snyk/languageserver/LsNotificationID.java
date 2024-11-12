package io.snyk.languageserver;

public interface LsNotificationID {
	public static final String SNYK_HAS_AUTHENTICATED = "$/snyk.hasAuthenticated";
	public static final String SNYK_IS_AVAILABLE_CLI = "$/snyk.isAvailableCli";
	public static final String SNYK_ADD_TRUSTED_FOLDERS = "$/snyk.addTrustedFolders";
	public static final String SNYK_SCAN = "$/snyk.scan";
	public static final String SNYK_PUBLISH_DIAGNOSTICS_316 = "$/snyk.publishDiagnostics316";
}