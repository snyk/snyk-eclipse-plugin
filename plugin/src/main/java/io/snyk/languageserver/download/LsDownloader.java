package io.snyk.languageserver.download;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.FileDownloadResponseHandler;
import io.snyk.eclipse.plugin.utils.LsMetadataResponseHandler;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsRuntimeEnvironment;
import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.AuthCache;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.impl.auth.BasicScheme;
import org.apache.http.impl.client.BasicAuthCache;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.conn.DefaultProxyRoutePlanner;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.stream.Collectors;

public class LsDownloader {
  private final CloseableHttpClient httpClient;
  private final LsRuntimeEnvironment runtimeEnvironment;
  private HttpClientContext context = HttpClientContext.create();
  private DefaultProxyRoutePlanner proxyRoutePlanner = null;
  private BasicCredentialsProvider credentialsProvider = null;
  private AuthScope authScope = null;
  private final ILog logger;

  private UsernamePasswordCredentials credentials;


  public LsDownloader(LsRuntimeEnvironment environment, HttpClientBuilder httpClientBuilder, IProxyData data, ILog logger) {
    this.runtimeEnvironment = environment;
    configure(httpClientBuilder, data);
    this.httpClient = httpClientBuilder.build();
    if (logger == null) {
      this.logger = Platform.getLog(getClass());
    } else {
      this.logger = logger;
    }
  }

  public void configure(HttpClientBuilder builder, IProxyData data) {
    if (data == null || data.getHost() == null) return;

    HttpHost proxy = new HttpHost(data.getHost(), data.getPort());
    this.proxyRoutePlanner = new DefaultProxyRoutePlanner(proxy);
    builder.setRoutePlanner(proxyRoutePlanner);

    if (data.getUserId() == null) return;

    this.credentialsProvider = new BasicCredentialsProvider();
    this.authScope = new AuthScope(proxy);
    this.credentials = new UsernamePasswordCredentials(data.getUserId(), data.getPassword());
    this.credentialsProvider.setCredentials(authScope, credentials);
    builder.setDefaultCredentialsProvider(this.credentialsProvider);

    AuthCache authCache = new BasicAuthCache();
    BasicScheme basicAuth = new BasicScheme();
    authCache.put(proxy, basicAuth);

    this.context = HttpClientContext.create();
    this.context.setCredentialsProvider(this.credentialsProvider);
    this.context.setAuthCache(authCache);
  }

  public void download(IProgressMonitor monitor) {
    File destinationFile = new File(Preferences.getInstance().getLsBinary());
    File tempFile = null;
    try {
      tempFile = File.createTempFile(destinationFile.getName(), ".tmp", destinationFile.getParentFile());
      logger.info("LS: Starting download to "+tempFile.getAbsolutePath());
      var version = getVersion();
      var expectedSha = getSha(version);

      logger.info("LS: Version: "+version+", Sha: "+expectedSha);

      LsDownloadRequest binaryRequest = new LsDownloadRequest(version, runtimeEnvironment);
      tempFile = httpClient.execute(binaryRequest, new FileDownloadResponseHandler(tempFile, monitor), context);
      logger.info("LS: Downloaded file.");

      var fileBytes = Files.readAllBytes(tempFile.toPath());
      verifyChecksum(expectedSha, fileBytes);
      logger.info("LS: Verified checksum.");

      try {
        logger.info("LS: Moving file to "+destinationFile.toPath());
        Files.move(tempFile.toPath(), destinationFile.toPath(), StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING);
        Preferences.getInstance().store(Preferences.LSP_VERSION, LsBinaries.REQUIRED_LSP_VERSION);
      } catch (AtomicMoveNotSupportedException e) {
        // fallback to renameTo because of e
        logger.warn("LS: Fallback using rename to "+destinationFile.toPath());
        if (!tempFile.renameTo(destinationFile))
          logger.error("LS: Rename failed: "+destinationFile.toPath());
          throw new IOException("Rename not successful");
      }
      logger.info("LS: Setting executable bit "+destinationFile.toPath());
      if (!destinationFile.setExecutable(true))
        throw new IOException("Could not set executable permission on " + destinationFile);
    } catch (IOException e) {
      logger.error("IOException", e);
      throw new RuntimeException(e);
    } finally {
      try {
        if (tempFile != null && tempFile.exists())
          if (!tempFile.delete())
            tempFile.deleteOnExit();
      } catch (Exception e) {
        SnykLogger.logError(e);
      }
    }
  }

  String getVersion() {
    LsVersionRequest req = new LsVersionRequest();
    LsMetadata metadata;
    try {
      metadata = httpClient.execute(req, new LsMetadataResponseHandler(), context);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    return metadata.getVersion();
  }

  String getSha(String version) {
    LsShaRequest shaRequest = new LsShaRequest(version);
    try (CloseableHttpResponse response = httpClient.execute(shaRequest, context)) {
      if (response.getStatusLine().getStatusCode() >= 400) {
        throw new RuntimeException("Download of Language Server failed. " + response.getStatusLine());
      }

      var entity = response.getEntity();
      try (var buf = new BufferedReader(new InputStreamReader(entity.getContent(), StandardCharsets.UTF_8))) {
        String fileName = runtimeEnvironment.getDownloadBinaryName(version);
        List<String> lines = buf.lines().filter(s -> s.contains(fileName)).collect(Collectors.toList());
        if (lines.size() != 1)
          throw new ChecksumVerificationException("Could not find sha for verification of file: " + fileName);
        return lines.get(0).split(" ")[0];
      }
    } catch (UnsupportedOperationException | IOException e) {
      throw new ChecksumVerificationException(e);
    }
  }

  private void verifyChecksum(String expectedSha, byte[] checksumDownloadedFile) {
    try {
      byte[] sha = MessageDigest.getInstance("SHA-256").digest(checksumDownloadedFile);
      String actualSha = bytesToHex(sha).toLowerCase();
      if (!actualSha.equalsIgnoreCase(expectedSha)) {
        throw new ChecksumVerificationException(
          "Expected " + expectedSha + ", but downloaded file has " + actualSha);
      }
    } catch (NoSuchAlgorithmException e) {
      throw new ChecksumVerificationException(e);
    }
  }

  private static String bytesToHex(byte[] hash) {
    StringBuilder hexString = new StringBuilder(2 * hash.length);
    for (byte b : hash) {
      String hex = Integer.toHexString(0xff & b);
      if (hex.length() == 1) {
        hexString.append('0');
      }
      hexString.append(hex);
    }
    return hexString.toString();
  }

  public DefaultProxyRoutePlanner getProxyRoutePlanner() {
    return proxyRoutePlanner;
  }

  public BasicCredentialsProvider getCredentialsProvider() {
    return credentialsProvider;
  }

  public AuthScope getAuthScope() {
    return authScope;
  }

  public UsernamePasswordCredentials getCredentials() {
    return credentials;
  }
}
