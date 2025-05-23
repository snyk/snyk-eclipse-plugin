package io.snyk.eclipse.plugin.preferences;

import java.util.List;

public final class AuthConstants {

	public static final String AUTH_OAUTH2 = "oauth";
	public static final String AUTH_PERSONAL_ACCESS_TOKEN = "pat";
	public static final String AUTH_API_TOKEN = "token";

	public static record AuthOptionData(String displayName, String value) {
	}

	public static final List<AuthOptionData> OPTION_DEFINITIONS = List.of(
			new AuthOptionData("OAuth2 (Recommended)", AUTH_OAUTH2),
			new AuthOptionData("Personal Access Token", AUTH_PERSONAL_ACCESS_TOKEN),
			new AuthOptionData("API Token (Legacy)", AUTH_API_TOKEN));

	public static String[][] getAuthOptions() {
		return OPTION_DEFINITIONS.stream().map(option -> new String[] { option.displayName(), option.value() })
				.toArray(String[][]::new);
	}

	private AuthConstants() {
		throw new IllegalStateException("Utility class");
	}
}