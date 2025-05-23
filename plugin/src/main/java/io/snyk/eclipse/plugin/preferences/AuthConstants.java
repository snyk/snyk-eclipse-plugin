package io.snyk.eclipse.plugin.preferences;

import java.util.List;

public final class AuthConstants {

    public static final String AUTH_OAUTH2 = "oauth" ;
    public static final String AUTH_PERSONAL_ACCESS_TOKEN = "pat" ;
    public static final String AUTH_API_TOKEN = "token";
    
    private record AuthOptionData(String displayName, String value) {}

    private static final List<AuthOptionData> OPTION_DEFINITIONS = List.of(
        new AuthOptionData("OAuth2", AUTH_OAUTH2),
        new AuthOptionData("Personal Access Token", AUTH_PERSONAL_ACCESS_TOKEN),
        new AuthOptionData("API Token", AUTH_API_TOKEN)
    );

    public static final String[][] AUTHENTICATION_OPTIONS = OPTION_DEFINITIONS.stream()
            .map(optionData -> new String[]{optionData.displayName(), optionData.value()})
            .toArray(String[][]::new);


    private AuthConstants() {
        throw new IllegalStateException("Utility class");
    }
}