package io.snyk.eclipse.plugin.html;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.languageserver.LsBaseTest;

class BaseHtmlProviderTest extends LsBaseTest {

	private BaseHtmlProvider htmlProvider;

	@BeforeEach
	@Override
	protected void setUp() {
		super.setUp();
		htmlProvider = new BaseHtmlProvider();
	}

	@Test
	void replaceCssVariables_replacesTextColor() {
		String html = "<div style=\"color: var(--text-color)\">Test</div>";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--text-color)"));
	}

	@Test
	void replaceCssVariables_replacesBackgroundColor() {
		String html = "<div style=\"background: var(--background-color)\">Test</div>";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--background-color)"));
	}

	@Test
	void replaceCssVariables_replacesBorderColor() {
		String html = "<div style=\"border: 1px solid var(--border-color)\">Test</div>";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--border-color)"));
	}

	@Test
	void replaceCssVariables_replacesLinkColor() {
		String html = "<a style=\"color: var(--link-color)\">Link</a>";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--link-color)"));
	}

	@Test
	void replaceCssVariables_replacesSectionBackgroundColor() {
		String html = "<div style=\"background: var(--section-background-color)\">Section</div>";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--section-background-color)"));
	}

	@Test
	void replaceCssVariables_replacesInputBackgroundColor() {
		String html = "<input style=\"background: var(--input-background-color)\">";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--input-background-color)"));
	}

	@Test
	void replaceCssVariables_replacesFocusColor() {
		String html = "<input style=\"outline-color: var(--focus-color)\">";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--focus-color)"));
	}

	@Test
	void replaceCssVariables_replacesNonce() {
		String html = "<style nonce=\"ideNonce\">body{}</style>";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("ideNonce"));
		assertTrue(result.contains("nonce=\""));
	}

	@Test
	void replaceCssVariables_withFixedFontSize_uses13px() {
		String html = "<body style=\"font-size: var(--main-font-size)\">Test</body>";

		String result = htmlProvider.replaceCssVariables(html, false);

		assertTrue(result.contains("13px"));
		assertFalse(result.contains("var(--main-font-size)"));
	}

	@Test
	void replaceCssVariables_withRelativeFontSize_usesRemUnit() {
		String html = "<body style=\"font-size: var(--main-font-size)\">Test</body>";

		String result = htmlProvider.replaceCssVariables(html, true);

		assertTrue(result.contains("rem"));
		assertFalse(result.contains("var(--main-font-size)"));
	}

	@Test
	void getNonce_returnsNonEmptyString() {
		String nonce = htmlProvider.getNonce();

		assertNotNull(nonce);
		assertFalse(nonce.isEmpty());
	}

	@Test
	void getNonce_returns32CharacterString() {
		String nonce = htmlProvider.getNonce();

		assertTrue(nonce.length() == 32);
	}

	@Test
	void getNonce_returnsSameValueOnSubsequentCalls() {
		String nonce1 = htmlProvider.getNonce();
		String nonce2 = htmlProvider.getNonce();

		assertTrue(nonce1.equals(nonce2));
	}
}
