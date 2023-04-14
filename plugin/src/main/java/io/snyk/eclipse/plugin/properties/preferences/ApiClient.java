package io.snyk.eclipse.plugin.properties.preferences;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.languageserver.LsRuntimeEnvironment;
import io.snyk.languageserver.download.HttpClientFactory;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.OAuthToken;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.impl.client.CloseableHttpClient;

import java.io.IOException;

public class ApiClient {
  private final CloseableHttpClient httpClient;

  ObjectMapper objectMapper = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

  private HttpClientContext context;

  public ApiClient() {
    this.httpClient = HttpClientFactory.getInstance().create(new LsRuntimeEnvironment());
    this.context = HttpClientFactory.getInstance().getContext();
  }

  public boolean checkSnykCodeEnablement() {
    try {
      Preferences prefs = Preferences.getInstance();
      if (prefs.getAuthToken().isBlank())
        return false;
      if (prefs.getPref(Preferences.ACTIVATE_SNYK_CODE).equals("false"))
        return false;

      String endpoint = prefs.getEndpoint();
      if (endpoint == null || endpoint.isBlank()) {
        endpoint = "https://snyk.io/api";
      }
      String url = "/cli-config/settings/sast";
      String org = prefs.getPref(Preferences.ORGANIZATION_KEY);
      if (org != null && !org.isBlank()) {
        url += "?org=" + org;
      }
      var httpGet = new HttpGet(endpoint + url);
      if (prefs.getPref(Preferences.AUTHENTICATION_METHOD).equals(Preferences.AUTH_METHOD_TOKEN)) {
        httpGet.addHeader("Authorization", "token " + prefs.getAuthToken());        
      } else {
        // first refresh token
        SnykExtendedLanguageClient.getInstance().refreshOAuthToken();
        var oauthToken = objectMapper.readValue(prefs.getAuthToken(), OAuthToken.class);
        httpGet.addHeader("Authorization", "bearer " + oauthToken.getAccessToken());
      }
      httpGet.addHeader("Content-Type", "application/json");
      var response = httpClient.execute(httpGet, context);

      CliConfigSettings cliConfigSettings = objectMapper.readValue(response.getEntity().getContent(),
          CliConfigSettings.class);
      boolean sastEnabled = cliConfigSettings.sastEnabled && !cliConfigSettings.localCodeEngine.enabled;
      String snykCodeEnablement = String.valueOf(sastEnabled);
      prefs.store(Preferences.ACTIVATE_SNYK_CODE, snykCodeEnablement);
      return sastEnabled;
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  @SuppressWarnings("unused")
  static class CliConfigSettings {
    public boolean sastEnabled;

    public LocalCodeEngine localCodeEngine;

    public boolean reportFalsePositivesEnabled;
  }

  /**
   * SAST local code engine configuration.
   */
  @SuppressWarnings("unused")
  static class LocalCodeEngine {
    boolean enabled;
  }
}
