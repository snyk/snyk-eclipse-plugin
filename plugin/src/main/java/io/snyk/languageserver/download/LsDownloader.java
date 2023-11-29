package io.snyk.languageserver.download;

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

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.impl.client.CloseableHttpClient;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.FileDownloadResponseHandler;
import io.snyk.eclipse.plugin.utils.LsMetadataResponseHandler;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsRuntimeEnvironment;

public class LsDownloader {
  private final CloseableHttpClient httpClient;
  private final LsRuntimeEnvironment runtimeEnvironment;
  private HttpClientContext context = HttpClientContext.create();
  private final ILog logger;

  public LsDownloader(HttpClientFactory factory, LsRuntimeEnvironment environment, ILog logger) {
    this.runtimeEnvironment = environment;
    this.httpClient = factory.create(environment);
    this.context = HttpClientFactory.getInstance().getContext();
    if (logger == null) {
      this.logger = Platform.getLog(getClass());
    } else {
      this.logger = logger;
    }
  }

  public void download(IProgressMonitor monitor) {
    File destinationFile = new File(Preferences.getInstance().getCliPath());
    File tempFile = null;
    try {
      Files.createDirectories(destinationFile.toPath().getParent());
      tempFile = File.createTempFile(destinationFile.getName(), ".tmp", destinationFile.getParentFile());
      logger.info("LS: Starting download to " + tempFile.getAbsolutePath());

      monitor.subTask("Determining version to download");
      var version = getVersion();
      monitor.worked(5);

      monitor.subTask("Determining SHA256");
      var expectedSha = getSha(version);
      monitor.worked(5);

      logger.info("LS: Version: " + version + ", Sha: " + expectedSha);

      monitor.subTask("Starting download of CLI version " + version);
      LsDownloadRequest binaryRequest = new LsDownloadRequest(version, runtimeEnvironment);
      tempFile = httpClient.execute(binaryRequest, new FileDownloadResponseHandler(tempFile, monitor), context);
      monitor.worked(80);
      logger.info("LS: Downloaded file.");

      monitor.subTask("Verifying checksum");
      var fileBytes = Files.readAllBytes(tempFile.toPath());
      verifyChecksum(expectedSha, fileBytes);
      monitor.worked(2);
      logger.info("LS: Verified checksum.");

      try {
        String message = "Moving file to " + destinationFile.toPath();
        monitor.subTask(message);
        logger.info("LS: " + message);
        Files.move(tempFile.toPath(), destinationFile.toPath(), StandardCopyOption.ATOMIC_MOVE,
            StandardCopyOption.REPLACE_EXISTING);
        monitor.worked(2);
        Preferences.getInstance().store(Preferences.LSP_VERSION, LsBinaries.REQUIRED_LS_PROTOCOL_VERSION);
      } catch (AtomicMoveNotSupportedException e) {
        // fallback to renameTo because of e
        logger.warn("LS: Fallback using rename to " + destinationFile.toPath());
        if (!tempFile.renameTo(destinationFile)) {
          logger.error("LS: Rename failed: " + destinationFile.toPath());
          throw new IOException("Rename not successful");
        }
      }
      monitor.subTask("Setting executable bit");
      logger.info("LS: Setting executable bit " + destinationFile.toPath());
      monitor.worked(5);
      if (!destinationFile.setExecutable(true))
        throw new IOException("Could not set executable permission on " + destinationFile);
    } catch (ChecksumVerificationException e) {
      throw e;
    } catch (Exception e) {
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
    var response = "";
    try {
      var req = new LsVersionRequest();
      response = httpClient.execute(req, new LsMetadataResponseHandler(), context);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    return response;
  }

  String getSha(String version) {
    LsShaRequest shaRequest = new LsShaRequest(version);
    try (CloseableHttpResponse response = httpClient.execute(shaRequest, context)) {
      if (response.getStatusLine().getStatusCode() >= 400) {
        throw new RuntimeException("Download of Language Server failed. " + response.getStatusLine());
      }

      var entity = response.getEntity();
      try (var buf = new BufferedReader(new InputStreamReader(entity.getContent(), StandardCharsets.UTF_8))) {
        String fileName = runtimeEnvironment.getDownloadBinaryName();
        List<String> lines = buf.lines().filter(s -> s.endsWith(fileName)).collect(Collectors.toList());
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
        throw new ChecksumVerificationException("Expected " + expectedSha + ", but downloaded file has " + actualSha);
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
}
