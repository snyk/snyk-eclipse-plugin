package io.snyk.eclipse.plugin.html;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Locale;

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

	@Test
	void replaceCssVariables_replacesVscodeScrollbarSliderBackground() {
		String html = "::-webkit-scrollbar-thumb { background: var(--vscode-scrollbarSlider-background); }";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--vscode-scrollbarSlider-background)"));
	}

	@Test
	void replaceCssVariables_replacesVscodeScrollbarSliderHoverBackground() {
		String html = "::-webkit-scrollbar-thumb:hover { background: var(--vscode-scrollbarSlider-hoverBackground); }";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--vscode-scrollbarSlider-hoverBackground)"));
	}

	@Test
	void replaceCssVariables_replacesVscodeScrollbarSliderActiveBackground() {
		String html = "::-webkit-scrollbar-thumb:active { background: var(--vscode-scrollbarSlider-activeBackground); }";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--vscode-scrollbarSlider-activeBackground)"));
	}

	@Test
	void replaceCssVariables_preservesUnknownTokenWithNestedParenFallback() {
		String html = "body { color: var(--vscode-unknown-token, rgba(255,0,0,0.5)); }";

		String result = htmlProvider.replaceCssVariables(html);

		assertTrue(result.contains("var(--vscode-unknown-token, rgba(255,0,0,0.5))"),
				"unknown vscode token with nested-paren fallback must be left untouched");
	}

	@Test
	void replaceCssVariables_leavesKnownTokenWithNestedParenFallbackUnresolved() {
		// By design: the regex uses [^()]* so a var() whose fallback itself contains parens
		// (e.g. a nested var() call) is intentionally NOT matched and left as-is.
		// Snyk-ls currently does not emit such forms, but this test documents the boundary.
		String html = "body { background: var(--vscode-editor-background, var(--fallback, #1e1e1e)); }";

		String result = htmlProvider.replaceCssVariables(html);

		assertTrue(result.contains("var(--vscode-editor-background, var(--fallback, #1e1e1e))"),
				"known vscode token with nested-paren fallback must be left untouched (by design)");
	}

	@Test
	void isDarkTheme_returnsFalseWhenDarkBackgroundColorAbsent() {
		// In test mode getColorAsHex always returns "" regardless of key → isDarkTheme() returns false
		assertFalse(htmlProvider.isDarkTheme());
	}

	@Test
	void isDarkTheme_returnsTrueWhenDarkBackgroundColorPresent() {
		BaseHtmlProvider darkProvider = new BaseHtmlProvider() {
			@Override
			public String getColorAsHex(String colorKey, String defaultColor) {
				if ("org.eclipse.ui.workbench.DARK_BACKGROUND".equals(colorKey)) {
					return "#2b2b2b";
				}
				return "";
			}
		};
		assertTrue(darkProvider.isDarkTheme());
	}

	@Test
	void replaceCssVariables_lightTheme_listHoverUsesBlackAlpha() {
		// In test mode isDarkTheme() returns false (light theme) → list hover must use black-alpha
		String html = "div { background: var(--list-hover-background); }";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--list-hover-background)"));
		assertTrue(result.contains("rgba(0, 0, 0"), "light-theme list hover must use black-alpha, not white-alpha");
	}

	@Test
	void replaceCssVariables_lightTheme_errorForegroundMeetsAA() {
		// In test mode isDarkTheme() returns false (light theme) → error-foreground must be dark red
		String html = "span { color: var(--error-foreground); }";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--error-foreground)"));
		assertTrue(result.contains("#a31515"), "light-theme error-foreground must be dark red (#a31515)");
	}

	@Test
	void replaceCssVariables_replacesVscodeListHoverBackground() {
		String html = "li:hover { background: var(--vscode-list-hoverBackground); }";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("var(--vscode-list-hoverBackground)"));
	}

	@Test
	void replaceCssVariables_nonce_replacesAttributeFormOnly() {
		// ideNonce must only be replaced in nonce attribute contexts, not in arbitrary body text
		String html = "<meta http-equiv='Content-Security-Policy' content=\"script-src 'nonce-ideNonce'\">"
				+ "<style nonce=\"ideNonce\">body{}</style>"
				+ "<p>The word ideNonce appears in content</p>";

		String result = htmlProvider.replaceCssVariables(html);

		assertFalse(result.contains("nonce=\"ideNonce\""), "nonce attr form must be replaced");
		assertFalse(result.contains("'nonce-ideNonce'"), "CSP nonce form must be replaced");
		// Text content form: the literal string "ideNonce" in body text is no longer rewritten
		assertTrue(result.contains("The word ideNonce appears in content"),
				"literal ideNonce in body text must NOT be rewritten");
	}

	@Test
	void replaceCssVariables_withRelativeFontSize_usesDecimalDotOnNonEnglishLocale() {
		Locale original = Locale.getDefault();
		try {
			Locale.setDefault(Locale.GERMAN);
			String html = "<body style=\"font-size: var(--main-font-size)\">Test</body>";

			String result = htmlProvider.replaceCssVariables(html, true);

			assertTrue(result.contains("rem"), "font-size must end with rem");
			assertFalse(result.matches(".*\\d,\\d+rem.*"), "font-size must use decimal dot, not comma (locale-independent)");
		} finally {
			Locale.setDefault(original);
		}
	}
}
