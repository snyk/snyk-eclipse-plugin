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
    var version = "20220303.140906";
    String binary = "snyk-ls_" + version + "_windows_amd64.exe";
    when(environmentMock.getDownloadBinaryName(version)).thenReturn(binary);

    LsDownloadRequest cut = new LsDownloadRequest(version, environmentMock);

    URI expectedUri = new URI(
        "https://static.snyk.io/snyk-ls/" + LsBinaries.REQUIRED_LS_PROTOCOL_VERSION + "/" + binary);
    assertEquals(expectedUri, cut.getURI());
    verify(environmentMock).getDownloadBinaryName(version);
  }
}
