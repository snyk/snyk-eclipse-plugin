package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import io.snyk.languageserver.LsBaseTest;

class TreeViewBrowserHandlerTest extends LsBaseTest {

	// T-U-002: setBrowserText(null) is a no-op guard
	@Test
	void setBrowserText_withNullHtml_isNoOp() {
		TreeViewBrowserHandler handler = new TreeViewBrowserHandler(null);
		// Must not throw
		handler.setBrowserText(null);
	}

	// T-U-003a: selectNode with null browser is a no-op (must not throw)
	@Test
	void selectNode_withNullBrowser_isNoOp() {
		TreeViewBrowserHandler handler = new TreeViewBrowserHandler(null);
		handler.selectNode("issue-id-with-'quote"); // must not throw
	}

	// T-U-003b: selectNode with null issueId is a no-op (must not throw)
	@Test
	void selectNode_withNullIssueId_isNoOp() {
		TreeViewBrowserHandler handler = new TreeViewBrowserHandler(null);
		handler.selectNode(null); // must not throw
	}

	// T-U-003c: selectNode with empty issueId is a no-op (must not throw)
	@Test
	void selectNode_withEmptyIssueId_isNoOp() {
		TreeViewBrowserHandler handler = new TreeViewBrowserHandler(null);
		handler.selectNode(""); // must not throw
	}

	// escapeJsSingleQuotedString handles all characters that would break a JS string literal
	@Test
	void escapeJsSingleQuotedString_escapesNewline() {
		String escaped = TreeViewBrowserHandler.escapeJsSingleQuotedString("id\nwith\nnewlines");
		assertFalse(escaped.contains("\n"), "escaped string must not contain literal newline");
		assertTrue(escaped.contains("\\n"), "escaped string must contain \\n");
	}

	@Test
	void escapeJsSingleQuotedString_escapesCarriageReturn() {
		String escaped = TreeViewBrowserHandler.escapeJsSingleQuotedString("id\rwith\rcr");
		assertFalse(escaped.contains("\r"), "escaped string must not contain literal CR");
		assertTrue(escaped.contains("\\r"), "escaped string must contain \\r");
	}

	@Test
	void escapeJsSingleQuotedString_escapesSingleQuote() {
		String escaped = TreeViewBrowserHandler.escapeJsSingleQuotedString("it's");
		assertTrue(escaped.contains("\\'"), "escaped string must escape single quotes");
	}

	@Test
	void escapeJsSingleQuotedString_escapesBackslash() {
		String escaped = TreeViewBrowserHandler.escapeJsSingleQuotedString("back\\slash");
		assertTrue(escaped.contains("\\\\"), "escaped string must escape backslashes");
	}

	// T-U-003: injectThemeCss replaces ${ideStyle} placeholder with theme style block
	@Test
	void injectThemeCss_replacesIdeStylePlaceholder() {
		String html = "<html><head>${ideStyle}</head><body></body></html>";
		String result = TreeViewBrowserHandler.injectThemeCss(html, null);
		assertTrue(result.contains("--vscode-foreground:"),
				"Injected HTML must contain --vscode-foreground:");
	}

	@Test
	void injectThemeCss_insertsBeforeHeadCloseWhenNoPlaceholder() {
		String html = "<html><head></head><body></body></html>";
		String result = TreeViewBrowserHandler.injectThemeCss(html, null);
		assertTrue(result.contains("--vscode-foreground:"),
				"Injected HTML must contain --vscode-foreground:");
		assertTrue(result.contains("</head>"), "Result must still contain </head>");
	}

	@Test
	void injectThemeCss_prependsStyleWhenNoHeadTag() {
		String html = "<html><body></body></html>";
		String result = TreeViewBrowserHandler.injectThemeCss(html, null);
		assertTrue(result.contains("--vscode-foreground:"),
				"Injected HTML must contain --vscode-foreground:");
	}

	@Test
	void injectThemeCss_withNullHtml_returnsNull() {
		String result = TreeViewBrowserHandler.injectThemeCss(null, null);
		assertTrue(result == null, "injectThemeCss with null html must return null");
	}
}
