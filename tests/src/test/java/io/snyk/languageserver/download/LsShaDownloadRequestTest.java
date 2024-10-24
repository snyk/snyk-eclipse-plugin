package io.snyk.languageserver.download;

import io.snyk.languageserver.LsBaseTest;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.URISyntaxException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.reset;

class LsShaDownloadRequestTest extends LsBaseTest {

  @Test
  void shouldDownloadFromSnyk() throws URISyntaxException {
    reset(environment);
    var version = "20220303.140906";

    LsShaRequest cut = new LsShaRequest(version);

    URI expectedUri = new URI("https://downloads.snyk.io/cli/v" + version
        + "/sha256sums.txt.asc");
    assertEquals(expectedUri, cut.getURI());
  }
}
