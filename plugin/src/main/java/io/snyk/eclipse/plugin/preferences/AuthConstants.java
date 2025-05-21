package io.snyk.eclipse.plugin.preferences;

public final class AuthConstants {

    public static final String AUTH_OAUTH2 = "oauth" ;
    public static final String AUTH_PERSONAL_ACCESS_TOKEN = "pat" ;
    public static final String AUTH_API_TOKEN = "token";
    
    
    public static final String[][] AUTHENTICATION_OPTIONS = new String[][] {
        new String[] { "OAuth2", AUTH_OAUTH2 },
        new String[] { "Personal Access Token", AUTH_PERSONAL_ACCESS_TOKEN },
        new String[] { "API Token", AUTH_API_TOKEN }
    };

    private AuthConstants() {
        throw new IllegalStateException("Utility class");
    }
}