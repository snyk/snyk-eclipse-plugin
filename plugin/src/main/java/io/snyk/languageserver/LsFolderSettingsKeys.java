package io.snyk.languageserver;

public final class LsFolderSettingsKeys {

	private LsFolderSettingsKeys() {
		throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
	}

	public static final String BASE_BRANCH = "base_branch";
	public static final String PREFERRED_ORG = "preferred_org";
	public static final String ADDITIONAL_PARAMETERS = "additional_parameters";
	public static final String ADDITIONAL_ENV = "additional_env";
	public static final String REFERENCE_FOLDER_PATH = "reference_folder_path";
	public static final String LOCAL_BRANCHES = "local_branches";
}
