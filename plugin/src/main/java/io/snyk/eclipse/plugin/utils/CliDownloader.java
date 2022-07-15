package io.snyk.eclipse.plugin.utils;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.snyk.eclipse.plugin.domain.LatestReleaseInfo;
import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.languageserver.LsRuntimeEnvironment;
import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.AuthCache;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.impl.auth.BasicScheme;
import org.apache.http.impl.client.BasicAuthCache;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.BasicResponseHandler;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.conn.DefaultProxyRoutePlanner;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.runtime.IProgressMonitor;

import java.io.File;
import java.io.IOException;
import java.util.Objects;

// TODO: Remove class in favor of Language Server downloading the CLI
public class CliDownloader {

  private static final String LATEST_RELEASES_URL = "https://api.github.com/repos/snyk/snyk/releases/latest";
  private static final String LATEST_RELEASE_DOWNLOAD_URL = "https://github.com/snyk/snyk/releases/download/%s/%s";
  private final CloseableHttpClient httpClient;

  public CliDownloader(HttpClientBuilder httpClientBuilder) {
    LsRuntimeEnvironment runtimeEnvironment = new LsRuntimeEnvironment(new Preferences());
    configure(httpClientBuilder, runtimeEnvironment.getProxyService().getProxyData(IProxyData.HTTPS_PROXY_TYPE));
    httpClient = httpClientBuilder.build();
  }

  public static CliDownloader newInstance(HttpClientBuilder httpClientBuilder) {
    return new CliDownloader(httpClientBuilder);
  }

  public File download(File destinationFile, IProgressMonitor monitor) {
    try {
      String snykWrapperFileName = Platform.current().snykWrapperFileName;
      String cliVersion = Objects.requireNonNull(getLatestReleaseInfo()).getTagName();

      HttpGet httpGet = new HttpGet(String.format(LATEST_RELEASE_DOWNLOAD_URL, cliVersion, snykWrapperFileName));

      return httpClient.execute(httpGet, new FileDownloadResponseHandler(destinationFile, monitor));
    } catch (Exception exception) {
      throw new IllegalStateException(exception);
    } finally {
      try {
        httpClient.close();
      } catch (IOException ioException) {
        SnykLogger.logError(ioException);
      }
    }
  }

  private LatestReleaseInfo getLatestReleaseInfo() {
    ObjectMapper objectMapper = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

    try {
      String latestReleaseInfoStr = requestLatestReleaseInfo();

      return objectMapper.readValue(latestReleaseInfoStr, LatestReleaseInfo.class);
    } catch (IOException ioException) {
      SnykLogger.logError(ioException);
    }

    return null;
  }

  private String requestLatestReleaseInfo() throws IOException {
    return httpClient.execute(new HttpGet(LATEST_RELEASES_URL), new BasicResponseHandler());
  }

  @SuppressWarnings("DuplicatedCode") // this whole class will go away, once we switch to language server completely
  private void configure(HttpClientBuilder builder, IProxyData data) {
    if (data == null || data.getHost() == null) return;

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

    var context = HttpClientContext.create();
    context.setCredentialsProvider(credentialsProvider);
    context.setAuthCache(authCache);
  }
}
