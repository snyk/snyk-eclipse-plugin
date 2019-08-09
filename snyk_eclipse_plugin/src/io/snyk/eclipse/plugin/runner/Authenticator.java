package io.snyk.eclipse.plugin.runner;

import static io.snyk.eclipse.plugin.utils.MockHandler.MOCK;

import java.io.IOException;
import java.net.URL;
import java.util.UUID;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.eclipse.ui.PlatformUI;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.exception.AuthException;
import io.snyk.eclipse.plugin.properties.Preferences;

public class Authenticator {
	
	public static final Authenticator INSTANCE = new Authenticator();
	
	private static final String API_URL = "https://snyk.io";
	
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
	
	// call login url and do callback
	public String callLogin() throws AuthException{
		String newToken = UUID.randomUUID().toString();
		
		

		
		String loginUri = new StringBuilder(getAuthUrlBase())
				.append("/login?token=").append(newToken)
				.append("&from=eclipsePlugin")
				.toString();
		
		try {		
			PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser().openURL(new URL(loginUri));			
			Thread.sleep(2000);
			return pollCallback(newToken);		
		} catch (Exception e) {
			throw new AuthException("Authentication problem: " + e.getMessage(),  e);
		}
		
	}
	
	

	private String pollCallback(String token) throws IOException, InterruptedException, AuthException {
		
		for (int i = 0; i <20; i++) {
	        String payload = "{\"token\" : \"" + token + "\"}";

	        StringEntity payloadEntity = new StringEntity(payload);
	
	        HttpClient httpClient = HttpClientBuilder.create().build();
	        HttpPost post = new HttpPost(getAuthUrlBase() + "/api/verify/callback");
	        post.setHeader("Content-type", "application/json");
	        post.setHeader("user-agent", "Needle/2.1.1 (Node.js v8.11.3; linux x64)");
	        post.setEntity(payloadEntity);
	
	        HttpResponse response = httpClient.execute(post);
	        String responseJson = EntityUtils.toString(response.getEntity(), "UTF-8");
	        AuthResponse authResponse = objectMapper.readValue(responseJson, AuthResponse.class);
	        if (authResponse.isOk()) {
	        	return authResponse.getApi();
	        }
	        Thread.sleep(2000);
		}
		
		throw new AuthException("Authentication timeout!");
		
	}
	
	public String getAuthUrlBase() throws AuthException {		
		String customEndpoint = Preferences.getEndpoint();
		if (customEndpoint == null || customEndpoint.isEmpty()) {
			return API_URL;
		}		
				
		try {
			URL endpoint = new URL(Preferences.getEndpoint());
			return endpoint.getProtocol() + "://" + endpoint.getAuthority();
		} catch (Exception e) {
			throw new AuthException("Authentication problem: " + e.getMessage(),  e);
		}	
	}
	
}
