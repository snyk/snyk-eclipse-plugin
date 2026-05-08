package io.snyk.languageserver.protocolextension;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.net.URISyntaxException;

import org.junit.jupiter.api.Test;

public class SnykShowFixUriDetailsTest {

	@Test
	void isValid_acceptsSnykCodeProduct() throws URISyntaxException {
		URI uri = new URI("snyk:///file?product=Snyk+Code&action=showInDetailPanel&issueId=abc");
		assertTrue(SnykShowFixUriDetails.fromURI(uri).isValid());
	}

	@Test
	void isValid_acceptsOssProduct() throws URISyntaxException {
		URI uri = new URI("snyk:///file?product=Snyk+Open+Source&action=showInDetailPanel&issueId=abc");
		assertTrue(SnykShowFixUriDetails.fromURI(uri).isValid());
	}

	@Test
	void isValid_acceptsIacProduct() throws URISyntaxException {
		URI uri = new URI("snyk:///file?product=Snyk+IaC&action=showInDetailPanel&issueId=abc");
		assertTrue(SnykShowFixUriDetails.fromURI(uri).isValid());
	}

	@Test
	void isValid_rejectsUnknownAction() throws URISyntaxException {
		URI uri = new URI("snyk:///file?product=Snyk+Code&action=unknownAction&issueId=abc");
		assertFalse(SnykShowFixUriDetails.fromURI(uri).isValid());
	}

	@Test
	void isValid_rejectsNonSnykScheme() throws URISyntaxException {
		URI uri = new URI("file:///file?product=Snyk+Code&action=showInDetailPanel&issueId=abc");
		assertFalse(SnykShowFixUriDetails.fromURI(uri).isValid());
	}

	@Test
	void isValid_rejectsNullProduct() throws URISyntaxException {
		URI uri = new URI("snyk:///file?action=showInDetailPanel&issueId=abc");
		assertFalse(SnykShowFixUriDetails.fromURI(uri).isValid());
	}

	@Test
	void isValid_rejectsEmptyProduct() throws URISyntaxException {
		URI uri = new URI("snyk:///file?product=&action=showInDetailPanel&issueId=abc");
		assertFalse(SnykShowFixUriDetails.fromURI(uri).isValid());
	}
}
