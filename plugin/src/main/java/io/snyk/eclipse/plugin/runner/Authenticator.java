package io.snyk.eclipse.plugin.runner;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.snyk.eclipse.plugin.exception.AuthException;
import io.snyk.eclipse.plugin.properties.Preferences;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLContextBuilder;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.util.EntityUtils;
import org.eclipse.ui.PlatformUI;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;
import java.io.IOException;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.UUID;

import static io.snyk.eclipse.plugin.utils.MockHandler.MOCK;

public class Authenticator {

  private static final Preferences PREFERENCES = new Preferences();

  public static final Authenticator INSTANCE = new Authenticator();

  private static final String API_URL = "https://snyk.io";

  private final SnykCliRunner cliRunner = new SnykCliRunner();
  ObjectMapper objectMapper = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

  private boolean isAuthenticated() throws AuthException {
    if (MOCK)
      return true;

    ProcessResult procesResult = cliRunner.snykConfig();
    if (procesResult.hasError()) {
      throw new AuthException(procesResult.getError());
    }

    if (procesResult.hasContentError()) {
      throw new AuthException(procesResult.getContent());
    }

    String content = procesResult.getContent();
    String authToken = PREFERENCES.getAuthToken();
    if (content != null && authToken != null) {
      return content.contains(authToken);
    }
    return false;
  }

  private void auth() throws AuthException {
    // don't authenticate using 'snyk auth'
//		ProcessResult procesResult = cliRunner.snykAuth();
//		if (procesResult.hasError()) {
//			throw new AuthException(procesResult.getError());
//		}
//
//		String content = procesResult.getContent();
//		if (content != null && content.contains("failed")) {
//			throw new AuthException(procesResult.getContent());
//		}
  }

  private void doAuthentication() throws AuthException {
    if (!isAuthenticated())
      auth();
  }

  // call login url and do callback
  private String callLogin() throws AuthException {
    String newToken = UUID.randomUUID().toString();

    String loginUri = getAuthUrlBase() + "/login?token=" + newToken +
      "&from=eclipsePlugin";

    try {
      PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser().openURL(new URL(loginUri));
      Thread.sleep(2000);
      return pollCallback(newToken);
    } catch (Exception e) {
      throw new AuthException("Authentication problem, " + e.getMessage(), e);
    }

  }

  private String pollCallback(String token) throws IOException, InterruptedException, AuthException,
    KeyManagementException, NoSuchAlgorithmException, KeyStoreException {

    //Only if insecure flag is active, create httpclient that accepts all ssl cert
    HttpClient httpClient = PREFERENCES.isInsecure() ? httpClientIgnoresCerts() : HttpClientBuilder.create().build();
    String payload = "{\"token\" : \"" + token + "\"}";

    StringEntity payloadEntity = new StringEntity(payload);

    HttpPost post = new HttpPost(getAuthUrlBase() + "/api/verify/callback");
    post.setHeader("Content-type", "application/json");
    post.setHeader("user-agent", "Needle/2.1.1 (Node.js v8.11.3; linux x64)");
    post.setEntity(payloadEntity);

    for (int i = 0; i < 20; i++) {
      HttpResponse response = httpClient.execute(post);
      String responseJson = EntityUtils.toString(response.getEntity(), "UTF-8");
      AuthResponse authResponse = objectMapper.readValue(responseJson, AuthResponse.class);
      if (authResponse.isOk()) {
        return authResponse.getApi();
      }
      Thread.sleep(2000);
    }

    throw new AuthException("timeout, please try again");

  }

  @SuppressWarnings("deprecation")
  private HttpClient httpClientIgnoresCerts() throws KeyManagementException, NoSuchAlgorithmException, KeyStoreException {
    HttpClientBuilder b = HttpClientBuilder.create();

    SSLContext sslContext = new SSLContextBuilder().loadTrustMaterial(null, (arg0, arg1) -> true).build();
    b.setSslcontext(sslContext);

    HostnameVerifier hostnameVerifier = SSLConnectionSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER;

    SSLConnectionSocketFactory sslSocketFactory = new SSLConnectionSocketFactory(sslContext, hostnameVerifier);
    Registry<ConnectionSocketFactory> socketFactoryRegistry = RegistryBuilder.<ConnectionSocketFactory>create()
      .register("http", PlainConnectionSocketFactory.getSocketFactory())
      .register("https", sslSocketFactory)
      .build();

    PoolingHttpClientConnectionManager connMgr = new PoolingHttpClientConnectionManager(socketFactoryRegistry);
    b.setConnectionManager(connMgr);

    return b.build();
  }

  private String getAuthUrlBase() throws AuthException {
    String customEndpoint = PREFERENCES.getEndpoint();
    if (customEndpoint == null || customEndpoint.isEmpty()) {
      return API_URL;
    }

    try {
      URL endpoint = new URL(PREFERENCES.getEndpoint());
      return endpoint.getProtocol() + "://" + endpoint.getAuthority();
    } catch (Exception e) {
      throw new AuthException("Authentication problem, " + e.getMessage(), e);
    }
  }

}
