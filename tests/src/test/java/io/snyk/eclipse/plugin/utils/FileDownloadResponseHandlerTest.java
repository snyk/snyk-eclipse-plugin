package io.snyk.eclipse.plugin.utils;

import org.apache.http.HttpEntity;
import org.apache.http.ProtocolVersion;
import org.apache.http.entity.BasicHttpEntity;
import org.apache.http.message.BasicHttpResponse;
import org.apache.http.message.BasicStatusLine;
import org.eclipse.core.runtime.IProgressMonitor;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class FileDownloadResponseHandlerTest {

  private static final String TEST_URL = "https://downloads.snyk.io/cli/v1.0.0/snyk-win-arm64.exe";

  @Test
  void shouldSaveFile() throws IOException {
    String payload = "test test test";
    BasicHttpResponse response = getBasicHttpResponse(200, "OK", payload);

    File tempFile = File.createTempFile("pre", "fix");
    tempFile.deleteOnExit();
    var cut = new FileDownloadResponseHandler(tempFile, mock(IProgressMonitor.class), TEST_URL);
    cut.handleResponse(response);

    var actual = Files.readString(tempFile.toPath());
    assertEquals(payload, actual);
  }

  @Test
  void shouldThrowOnHttp404WithUrlAndStatus() throws IOException {
    String errorBody = "<html>Not Found</html>";
    BasicHttpResponse response = getBasicHttpResponse(404, "Not Found", errorBody);

    File tempFile = File.createTempFile("pre", "fix");
    tempFile.deleteOnExit();
    var cut = new FileDownloadResponseHandler(tempFile, mock(IProgressMonitor.class), TEST_URL);

    IOException exception = assertThrows(IOException.class, () -> cut.handleResponse(response));
    assertTrue(exception.getMessage().contains("404"), "Should contain status code");
    assertTrue(exception.getMessage().contains(TEST_URL), "Should contain URL");
  }

  @Test
  void shouldThrowOnHttp500WithUrlAndStatus() throws IOException {
    String errorBody = "Internal Server Error";
    BasicHttpResponse response = getBasicHttpResponse(500, "Internal Server Error", errorBody);

    File tempFile = File.createTempFile("pre", "fix");
    tempFile.deleteOnExit();
    var cut = new FileDownloadResponseHandler(tempFile, mock(IProgressMonitor.class), TEST_URL);

    IOException exception = assertThrows(IOException.class, () -> cut.handleResponse(response));
    assertTrue(exception.getMessage().contains("500"), "Should contain status code");
    assertTrue(exception.getMessage().contains(TEST_URL), "Should contain URL");
  }

  private BasicHttpResponse getBasicHttpResponse(int statusCode, String reasonPhrase, String payload) {
    var response =
      new BasicHttpResponse(new BasicStatusLine(new ProtocolVersion("https", 1, 1), statusCode, reasonPhrase));
    HttpEntity entity = getHttpEntity(payload);
    response.setEntity(entity);
    return response;
  }

  private HttpEntity getHttpEntity(String payload) {
    BasicHttpEntity entity = new BasicHttpEntity();
    entity.setContentLength(payload.length());
    entity.setContent(new ByteArrayInputStream(payload.getBytes()));
    return entity;
  }
}
