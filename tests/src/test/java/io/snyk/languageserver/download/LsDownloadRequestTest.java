package io.snyk.languageserver.download;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.URISyntaxException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

class LsDownloadRequestTest extends LsBaseTest {

	@Test
	void shouldDownloadFromGithubWithOsDetectionByDefault() throws URISyntaxException {
		var version = "20220303.140906";
		String binary = "snyk-ls_" + version + "_windows_amd64.exe";
		when(utils.getDownloadBinaryName(version)).thenReturn(binary);

		LsDownloadRequest cut = new LsDownloadRequest(version, utils);

		URI expectedUri = new URI("https://static.snyk.io/snyk-ls/" + binary);
		assertEquals(expectedUri, cut.getURI());
		verify(utils).getDownloadBinaryName(version);
	}
}