package io.snyk.languageserver.download;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.URISyntaxException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class LsShaDownloadRequestTest extends LsBaseTest {

    @Test
    void shouldDownloadFromGithubWithOsDetectionByDefault() throws URISyntaxException {
        reset(utils);
        when(utils.getBinaryName(any(), any())).thenReturn("snyk-lsp.linux.amd64");

        LsShaDownloadRequest cut = new LsShaDownloadRequest(utils);

        URI expectedUri = new URI("https://github.com/snyk/snyk-lsp/releases/download/latest/snyk-lsp.linux.amd64.sha256");
        assertEquals(expectedUri, cut.getURI());
        verify(utils).getBinaryName(any(), any());
    }
}