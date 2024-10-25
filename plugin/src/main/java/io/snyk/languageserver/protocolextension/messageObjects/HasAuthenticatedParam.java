package io.snyk.languageserver.protocolextension.messageObjects;

public class HasAuthenticatedParam {
	private String token;
	private String apiUrl;

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public String getApiUrl() {
		return apiUrl;
	}

	public void setApiUrl(String apiUrl) {
		this.apiUrl = apiUrl;
	}
}
