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
        reset(utils);
        var version = "20220303.140906";

        LsShaRequest cut = new LsShaRequest(version, utils);

        URI expectedUri = new URI("https://static.snyk.io/snyk-ls/snyk-ls_20220303.140906_SHA256SUMS");
        assertEquals(expectedUri, cut.getURI());
    }
}