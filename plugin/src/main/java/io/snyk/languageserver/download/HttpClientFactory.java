package io.snyk.languageserver.download;

import io.snyk.languageserver.LsRuntimeEnvironment;
import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.AuthCache;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.impl.auth.BasicScheme;
import org.apache.http.impl.client.BasicAuthCache;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.DefaultProxyRoutePlanner;
import org.eclipse.core.net.proxy.IProxyData;

public class HttpClientFactory {
  private static HttpClientFactory instance;
  private CloseableHttpClient client;
  private HttpClientContext context = HttpClientContext.create();;

  public static HttpClientFactory getInstance() {
    if (instance == null) {
      instance = new HttpClientFactory();
    }
    return instance;
  }

  public CloseableHttpClient create(LsRuntimeEnvironment runtimeEnvironment) {
    if (client != null) return client;
    var httpClientBuilder = HttpClients.custom();
    IProxyData[] proxyData = runtimeEnvironment.getProxyService().select(LsBinaries.getBaseUri());
    var relevantProxyData = getRelevantProxyData(proxyData);
    configure(httpClientBuilder, relevantProxyData);
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
