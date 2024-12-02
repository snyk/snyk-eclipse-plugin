package io.snyk.languageserver.download;

import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;

import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.AuthCache;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.TrustStrategy;
import org.apache.http.impl.auth.BasicScheme;
import org.apache.http.impl.client.BasicAuthCache;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.DefaultProxyRoutePlanner;
import org.apache.http.ssl.SSLContexts;
import org.eclipse.core.net.proxy.IProxyData;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsRuntimeEnvironment;

public class HttpClientFactory {
  private static HttpClientFactory instance;
  private final HttpClientContext context = HttpClientContext.create();

  public static HttpClientFactory getInstance() {
    if (instance == null) {
      instance = new HttpClientFactory();
    }
    return instance;
  }

  public CloseableHttpClient create(LsRuntimeEnvironment runtimeEnvironment) {
    var httpClientBuilder = HttpClients.custom();

    IProxyData[] proxyData = runtimeEnvironment.getProxyService().select(LsBinaries.getBaseUri());
    var relevantProxyData = getRelevantProxyData(proxyData);
    configure(httpClientBuilder, relevantProxyData);

    if (Preferences.getInstance().isInsecure()) {
      try {
        TrustStrategy acceptingTrustStrategy = (certificate, authType) -> true;
        SSLContext sslContext = SSLContexts.custom().loadTrustMaterial(null, acceptingTrustStrategy).build();
        httpClientBuilder.setSSLContext(sslContext);
        HostnameVerifier allowAllHosts = new NoopHostnameVerifier();
        SSLConnectionSocketFactory connectionFactory = new SSLConnectionSocketFactory(sslContext, allowAllHosts);
        httpClientBuilder.setSSLSocketFactory(connectionFactory);
      } catch (NoSuchAlgorithmException | KeyStoreException | KeyManagementException e) {
        SnykLogger.logError(e);
      }
    }
    return httpClientBuilder.build();
  }

  IProxyData getRelevantProxyData(IProxyData[] proxyData) {
    for (IProxyData data : proxyData) {
      if (data.getHost() == null)
        continue;
      return data;
    }
    return null;
  }

  void configure(HttpClientBuilder builder, IProxyData data) {
    if (data == null || data.getHost() == null) return ;

    HttpHost proxy = new HttpHost(data.getHost(), data.getPort());
    var proxyRoutePlanner = new DefaultProxyRoutePlanner(proxy);
    builder.setRoutePlanner(proxyRoutePlanner);

    if (data.getUserId() == null) return;

    var credentialsProvider = new BasicCredentialsProvider();
    var authScope = new AuthScope(proxy);
    var credentials = new UsernamePasswordCredentials(data.getUserId(), data.getPassword());
    credentialsProvider.setCredentials(authScope, credentials);
    builder.setDefaultCredentialsProvider(credentialsProvider);

    AuthCache authCache = new BasicAuthCache();
    BasicScheme basicAuth = new BasicScheme();
    authCache.put(proxy, basicAuth);
    context.setCredentialsProvider(credentialsProvider);
    context.setAuthCache(authCache);
  }

  public HttpClientContext getContext() {
    return context;
  }
}
