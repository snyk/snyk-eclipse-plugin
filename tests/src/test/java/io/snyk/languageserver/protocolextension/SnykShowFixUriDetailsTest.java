package io.snyk.languageserver.protocolextension;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_OSS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.FILTERABLE_ISSUE_TYPE_TO_DISPLAY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.FILTERABLE_ISSUE_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.FILTERABLE_ISSUE_INFRASTRUCTURE_AS_CODE;
import static io.snyk.eclipse.plugin.domain.ProductConstants.FILTERABLE_ISSUE_OPEN_SOURCE;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
	void isValid_acceptsSecretsProduct() throws URISyntaxException {
		URI uri = new URI("snyk:///file?product=Snyk+Secrets&action=showInDetailPanel&issueId=abc");
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

	// FILTERABLE_ISSUE_TYPE_TO_DISPLAY maps filterableIssueType() to the DISPLAYED_* format
	// that ContentRootNode.getProductNode() expects
	@Test
	void filterableIssueTypeToDisplay_oss() {
		assertEquals(DISPLAYED_OSS, FILTERABLE_ISSUE_TYPE_TO_DISPLAY.get(FILTERABLE_ISSUE_OPEN_SOURCE));
	}

	@Test
	void filterableIssueTypeToDisplay_code() {
		assertEquals(DISPLAYED_CODE_SECURITY, FILTERABLE_ISSUE_TYPE_TO_DISPLAY.get(FILTERABLE_ISSUE_CODE_SECURITY));
	}

	@Test
	void filterableIssueTypeToDisplay_iac() {
		assertEquals(DISPLAYED_IAC, FILTERABLE_ISSUE_TYPE_TO_DISPLAY.get(FILTERABLE_ISSUE_INFRASTRUCTURE_AS_CODE));
	}
}
