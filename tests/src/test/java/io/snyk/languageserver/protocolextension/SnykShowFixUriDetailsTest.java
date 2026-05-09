package io.snyk.languageserver.protocolextension;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DIAGNOSTIC_SOURCE_SNYK_CODE;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DIAGNOSTIC_SOURCE_SNYK_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DIAGNOSTIC_SOURCE_SNYK_OSS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
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

	// normalizeProductCodename maps LS short codenames to Eclipse display names
	@Test
	void normalizeProductCodename_oss() {
		assertEquals(DIAGNOSTIC_SOURCE_SNYK_OSS, SnykExtendedLanguageClient.normalizeProductCodename("oss"));
	}

	@Test
	void normalizeProductCodename_code() {
		assertEquals(DIAGNOSTIC_SOURCE_SNYK_CODE, SnykExtendedLanguageClient.normalizeProductCodename("code"));
	}

	@Test
	void normalizeProductCodename_iac() {
		assertEquals(DIAGNOSTIC_SOURCE_SNYK_IAC, SnykExtendedLanguageClient.normalizeProductCodename("iac"));
	}

	@Test
	void normalizeProductCodename_longFormPassthrough() {
		assertEquals(DIAGNOSTIC_SOURCE_SNYK_OSS,
				SnykExtendedLanguageClient.normalizeProductCodename(DIAGNOSTIC_SOURCE_SNYK_OSS));
	}

	@Test
	void normalizeProductCodename_nullReturnsNull() {
		assertNull(SnykExtendedLanguageClient.normalizeProductCodename(null));
	}
}
