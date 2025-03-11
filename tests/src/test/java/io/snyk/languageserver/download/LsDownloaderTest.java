package io.snyk.languageserver.download;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.apache.http.HttpEntity;
import org.apache.http.StatusLine;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.protocol.HttpContext;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.FileDownloadResponseHandler;
import io.snyk.eclipse.plugin.utils.LsMetadataResponseHandler;
import io.snyk.languageserver.LsBaseTest;

public class LsDownloaderTest extends LsBaseTest {
  CloseableHttpClient httpClient = null;
  HttpClientFactory httpClientFactory = null;

  @BeforeEach
  @Override
  protected void setUp() {
    super.setUp();
    httpClient = mock(CloseableHttpClient.class);
    httpClientFactory = mock(HttpClientFactory.class);

    when(httpClientFactory.create(any())).thenReturn(httpClient);
    when(proxyServiceMock.select(any())).thenReturn(new IProxyData[0]);
    try {
      when(
          httpClient.execute(any(LsVersionRequest.class), any(LsMetadataResponseHandler.class), any(HttpContext.class)))
          .thenReturn("1.1234.0");
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  void downloadShouldFailWhenShaWrongAndFileShouldNotBeOverwritten() throws IOException {
    String originalCLIPath = Preferences.getInstance().getCliPath();
    try {
      byte[] expectedLsContent = "original file content".getBytes();
      String testCliPath = File.createTempFile("eclipse-test", "").toPath().toString();
      Preferences.getInstance().store(Preferences.CLI_PATH, testCliPath);
      Files.write(new File(testCliPath).toPath(), expectedLsContent);

      LsDownloader cut = new LsDownloader(httpClientFactory, environment, mock(ILog.class));
      File testFile = File.createTempFile("download-test", "tmp");
      testFile.deleteOnExit();

      // sha corresponds to file content (`echo "test 123" | sha256sum`)
      Files.write(testFile.toPath(), "test 123".getBytes(StandardCharsets.UTF_8));
      InputStream shaStream = new ByteArrayInputStream("wrong sha".getBytes());
      mockShaStream(shaStream);
      when(httpClient.execute(any(LsDownloadRequest.class), any(FileDownloadResponseHandler.class),
          any(HttpContext.class))).thenReturn(testFile);

      assertThrows(ChecksumVerificationException.class, () -> cut.download(mock(IProgressMonitor.class)));

      File lsFile = new File(testCliPath);
      assertTrue(lsFile.exists());
      assertArrayEquals(Files.readAllBytes(lsFile.toPath()), expectedLsContent);
      verify(httpClient, times(1)).execute(any(LsVersionRequest.class), any(LsMetadataResponseHandler.class),
          any(HttpContext.class));
      verify(httpClient, times(1)).execute(any(LsShaRequest.class), any(HttpContext.class));
    } finally {
      Preferences.getInstance().store(Preferences.CLI_PATH, originalCLIPath);
    }
  }

  @Test
  void download_whenFinished_updatesLSPVersion() throws IOException {
    LsDownloader cut = new LsDownloader(httpClientFactory, environment, mock(ILog.class));
    Path source = Paths.get("src/test/resources/ls-dummy-binary");
    var testFile = Files.createTempFile("download-test", "tmp").toFile();
    testFile.deleteOnExit();
    Files.copy(source, testFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
    var checksums = "1a69d74e3b70cd2d386145bf6eb3882196525b7ca2b42e3fc87c67de873abf72  snyk-ls_testVersion_windows_386.exe\n"
        + "2b418a5d0573164b4f93188fc94de0332fc0968e7a8439b01f530a4cdde1dcf2  snyk-ls_testVersion_linux_amd64\n"
        + "5f53be6dac86284d58322dff0dd6b9e3a6002e593d3a16fc61e23d82b428915a  snyk-ls_testVersion_darwin_arm64\n";
    InputStream shaStream = new ByteArrayInputStream(checksums.getBytes());
    mockShaStream(shaStream);
    when(httpClient.execute(any(LsDownloadRequest.class), any(FileDownloadResponseHandler.class),
        any(HttpContext.class))).thenReturn(testFile);

    cut.download(mock(IProgressMonitor.class));

    assertEquals(Preferences.getInstance().getLspVersion(), LsBinaries.REQUIRED_LS_PROTOCOL_VERSION);
  }

  @Test
  void downloadShouldIssueDownloadRequestForShaAndBinary() throws IOException {
    LsDownloader cut = new LsDownloader(httpClientFactory, environment, mock(ILog.class));
    Path source = Paths.get("src/test/resources/ls-dummy-binary");
    var testFile = Files.createTempFile("download-test", "tmp").toFile();
    testFile.deleteOnExit();
    Files.copy(source, testFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
    var fileContent = Files.readAllBytes(testFile.toPath());
    var checksums = "1a69d74e3b70cd2d386145bf6eb3882196525b7ca2b42e3fc87c67de873abf72  snyk-ls_testVersion_windows_386.exe\n"
        + "2b418a5d0573164b4f93188fc94de0332fc0968e7a8439b01f530a4cdde1dcf2  snyk-ls_testVersion_linux_amd64\n"
        + "5f53be6dac86284d58322dff0dd6b9e3a6002e593d3a16fc61e23d82b428915a  snyk-ls_testVersion_darwin_arm64\n";
    InputStream shaStream = new ByteArrayInputStream(checksums.getBytes());
    mockShaStream(shaStream);
    when(httpClient.execute(any(LsDownloadRequest.class), any(FileDownloadResponseHandler.class),
        any(HttpContext.class))).thenReturn(testFile);

    cut.download(mock(IProgressMonitor.class));

    verify(httpClient, times(1)).execute(any(LsVersionRequest.class), any(LsMetadataResponseHandler.class),
        any(HttpContext.class));
    verify(httpClient, times(1)).execute(any(LsShaRequest.class), any(HttpContext.class));
    verify(httpClient, times(1)).execute(any(LsDownloadRequest.class), any(FileDownloadResponseHandler.class),
        any(HttpContext.class));
    File lsFile = new File(Preferences.getInstance().getCliPath());
    assertTrue(lsFile.exists());
    assertArrayEquals(Files.readAllBytes(lsFile.toPath()), fileContent);
  }

  @Test
  void respectsLSBinaryPath() throws IOException {
    String lsBinaryPath = getTempFile().toString();
    Files.delete(Path.of(lsBinaryPath));
    Preferences.getInstance().store(Preferences.CLI_PATH, lsBinaryPath);
    LsDownloader cut = new LsDownloader(httpClientFactory, environment, mock(ILog.class));
    Path source = Paths.get("src/test/resources/ls-dummy-binary");
    var testFile = Files.createTempFile("download-test", "tmp").toFile();
    testFile.deleteOnExit();
    Files.copy(source, testFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
    var fileContent = Files.readAllBytes(testFile.toPath());
    var checksums = "1a69d74e3b70cd2d386145bf6eb3882196525b7ca2b42e3fc87c67de873abf72  snyk-ls_testVersion_windows_386.exe\n"
        + "2b418a5d0573164b4f93188fc94de0332fc0968e7a8439b01f530a4cdde1dcf2  snyk-ls_testVersion_linux_amd64\n"
        + "5f53be6dac86284d58322dff0dd6b9e3a6002e593d3a16fc61e23d82b428915a  snyk-ls_testVersion_darwin_arm64\n";
    InputStream shaStream = new ByteArrayInputStream(checksums.getBytes());
    mockShaStream(shaStream);
    when(httpClient.execute(any(LsDownloadRequest.class), any(FileDownloadResponseHandler.class),
        any(HttpContext.class))).thenReturn(testFile);

    cut.download(mock(IProgressMonitor.class));

    File lsFile = new File(lsBinaryPath);
    assertTrue(lsFile.exists());
    assertArrayEquals(Files.readAllBytes(lsFile.toPath()), fileContent);
  }

  @Test
  void getVersionShouldDownloadAndExtractTheLatestVersion() {
    LsDownloader cut = new LsDownloader(httpClientFactory, environment, mock(ILog.class));
    var expected = "1.1234.0";
    var actual = cut.getVersion("stable");
    assertEquals(expected, actual);
  }

  private void mockShaStream(InputStream shaStream) throws IOException {
    CloseableHttpResponse shaHttpResponseMock = mock(CloseableHttpResponse.class);
    when(httpClient.execute(any(LsShaRequest.class), any(HttpContext.class))).thenReturn(shaHttpResponseMock);
    when(httpClient.execute(any(LsShaRequest.class))).thenReturn(shaHttpResponseMock);
    StatusLine statusLineMock = mock(StatusLine.class);
    when(shaHttpResponseMock.getStatusLine()).thenReturn(statusLineMock);
    when(statusLineMock.getStatusCode()).thenReturn(200);
    HttpEntity shaHttpEntityMock = mock(HttpEntity.class);
    when(shaHttpResponseMock.getEntity()).thenReturn(shaHttpEntityMock);
    when(shaHttpEntityMock.getContent()).thenReturn(shaStream);
  }

}
