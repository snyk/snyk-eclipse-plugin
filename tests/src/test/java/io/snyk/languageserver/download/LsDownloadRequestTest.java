package io.snyk.languageserver.download;

import io.snyk.languageserver.LsBaseTest;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.URISyntaxException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class LsDownloadRequestTest extends LsBaseTest {

  @Test
  void shouldDownloadFromGithubWithOsDetectionByDefault() throws URISyntaxException {
    String binary = "snyk-win.exe";
    when(environment.getDownloadBinaryName()).thenReturn(binary);

    String version = "1.1234.0";
    LsDownloadRequest cut = new LsDownloadRequest(version, environment);

    URI expectedUri = new URI(
        "https://static.snyk.io/cli/v" + version + "/" + binary);
    assertEquals(expectedUri, cut.getURI());
    verify(environment).getDownloadBinaryName();
  }
}
