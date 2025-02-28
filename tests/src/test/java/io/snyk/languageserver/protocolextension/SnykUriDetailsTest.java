package io.snyk.languageserver.protocolextension;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.net.URISyntaxException;

import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.domain.ProductConstants;

public class SnykUriDetailsTest {

	private static final String BASE_URI_STRING = 
			"snyk:///test_repos/project-with-vulns/routes/vulnCodeSnippet.ts?product=Snyk+Code&issueId=id&action=showInDetailPanel";

	private URI createSnykUri() throws URISyntaxException {
		return new URI(BASE_URI_STRING);
	}

	@Test
	void testFromURI() throws URISyntaxException {
		SnykUriDetails details = SnykUriDetails.fromURI(createSnykUri());

		assertTrue(details.scheme().equals("snyk"));
		assertTrue(details.filePath().equals(
				"/test_repos/project-with-vulns/routes/vulnCodeSnippet.ts"));
		assertTrue(details.product()
				.equals(ProductConstants.DIAGNOSTIC_SOURCE_SNYK_CODE));
		assertTrue(details.action().equals("showInDetailPanel"));
		assertTrue(details.issueId().equals("id"));
	}

	@Test
	void testFromURINullProduct() throws URISyntaxException {
		URI uri = new URI("snyk:///test_repos/project-with-vulns/routes/vulnCodeSnippet.ts?issueId=id&action=showInDetailPanel");
		SnykUriDetails details = SnykUriDetails.fromURI(uri);

		assertTrue(details.scheme().equals("snyk"));
		assertTrue(details.filePath().equals("/test_repos/project-with-vulns/routes/vulnCodeSnippet.ts"));
		assertTrue(details.product() == null); // Check if product is null
		assertTrue(details.action().equals("showInDetailPanel"));
		assertTrue(details.issueId().equals("id"));
	}

	@Test
	void testFromURIEmptyProduct() throws URISyntaxException {
		SnykUriDetails details = SnykUriDetails.fromURI(createSnykUri());

		assertTrue(details.scheme().equals("snyk"));
		assertTrue(details.filePath().equals("/test_repos/project-with-vulns/routes/vulnCodeSnippet.ts"));
		assertFalse(details.product().isEmpty()); // Check if product is not
													// empty
		assertTrue(details.action().equals("showInDetailPanel"));
		assertTrue(details.issueId().equals("id"));
	}

	@Test
	void testIsValidTrue() throws URISyntaxException {
		SnykUriDetails details = SnykUriDetails.fromURI(createSnykUri());

		assertTrue(details.isValid());
	}

	@Test
	void testIsValidFalse() throws URISyntaxException {
		URI uri = new URI("http://path/product/showInDetailPanel?issueId=id");
		SnykUriDetails details = SnykUriDetails.fromURI(uri);

		assertFalse(details.isValid());
	}

	@Test
	void testIsValidProductFalse() throws URISyntaxException {
		URI uri = new URI("snyk://path/null/showInDetailPanel?issueId=id");
		SnykUriDetails details = SnykUriDetails.fromURI(uri);

		assertFalse(details.isValid());
	}
}
