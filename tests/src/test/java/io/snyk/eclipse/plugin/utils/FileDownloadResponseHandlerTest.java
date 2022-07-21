package io.snyk.eclipse.plugin.utils;

import io.snyk.languageserver.LsBaseTest;
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
import static org.mockito.Mockito.mock;

class FileDownloadResponseHandlerTest extends LsBaseTest {

  @Test
  void shouldSaveFile() throws IOException {
    String payload = "test test test";
    BasicHttpResponse response = getBasicHttpResponse(payload);

    File tempFile = File.createTempFile("pre", "fix");
    tempFile.deleteOnExit();
    var cut = new FileDownloadResponseHandler(tempFile, mock(IProgressMonitor.class));
    cut.handleResponse(response);

    var actual = Files.readString(tempFile.toPath());
    assertEquals(payload, actual);
  }

  private BasicHttpResponse getBasicHttpResponse(String payload) {
    var response =
      new BasicHttpResponse(new BasicStatusLine(new ProtocolVersion("https", 1, 1), 200, "OK"));
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
