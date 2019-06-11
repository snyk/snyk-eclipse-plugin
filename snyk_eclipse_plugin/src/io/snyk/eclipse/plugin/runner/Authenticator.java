package io.snyk.eclipse.plugin.runner;

import static io.snyk.eclipse.plugin.utils.MockHandler.MOCK;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.exception.AuthException;
import io.snyk.eclipse.plugin.properties.Preferences;

public class Authenticator {
	
	public static Authenticator INSTANCE = new Authenticator();
	
	private SnykCliRunner cliRunner = new SnykCliRunner();
	ObjectMapper objectMapper = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
	
	
	public boolean isAuthenticated() throws AuthException {
		if(MOCK) return true;
		
		ProcessResult procesResult = cliRunner.snykConfig();
		if (procesResult.hasError()) {
			throw new AuthException(procesResult.getError());
		}
		
		if (procesResult.hasContentError()) {
			throw new AuthException(procesResult.getContent());
		}
		
	
		String content = procesResult.getContent();
		String authToken = Preferences.getAuthToken();
		if (content != null && authToken != null) {
			return content.contains(authToken);
		}
		return false;
	}
	
	public void auth() throws AuthException {
		ProcessResult procesResult = cliRunner.snykAuth();
		if (procesResult.hasError()) {
			throw new AuthException(procesResult.getError());
		}
		
		String content = procesResult.getContent();
		if (content != null && content.contains("failed") ) {
			throw new AuthException(procesResult.getContent());
		}
	}
	
	public void doAuthentication() throws AuthException {
		if (!isAuthenticated()) auth();
	}
	

	

}
