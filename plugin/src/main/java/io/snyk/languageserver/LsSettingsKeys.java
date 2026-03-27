package io.snyk.languageserver;

public final class LsSettingsKeys {

	private LsSettingsKeys() {
		throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
	}

	public static final String ENDPOINT = "endpoint";
	public static final String ORGANIZATION = "organization";
	public static final String TOKEN = "token";
	public static final String ACTIVATE_SNYK_CODE = "activateSnykCode";
	public static final String ACTIVATE_SNYK_OPEN_SOURCE = "activateSnykOpenSource";
	public static final String ACTIVATE_SNYK_IAC = "activateSnykIac";
	public static final String INSECURE = "insecure";
	public static final String ADDITIONAL_PARAMS = "additionalParams";
	public static final String PATH = "path";
	public static final String SCANNING_MODE = "scanningMode";
}
