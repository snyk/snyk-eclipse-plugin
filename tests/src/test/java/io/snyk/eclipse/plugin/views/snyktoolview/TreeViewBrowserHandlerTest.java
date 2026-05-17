package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import io.snyk.languageserver.LsBaseTest;

class TreeViewBrowserHandlerTest extends LsBaseTest {

	// setBrowserText(null) is a no-op guard
	@Test
	void setBrowserText_withNullHtml_isNoOp() {
		TreeViewBrowserHandler handler = new TreeViewBrowserHandler(null);
		handler.setBrowserText(null);
	}

	// selectNode with null browser is a no-op (must not throw)
	@Test
	void selectNode_withNullBrowser_isNoOp() {
		TreeViewBrowserHandler handler = new TreeViewBrowserHandler(null);
		handler.selectNode("issue-id-with-'quote");
	}

	@Test
	void selectNode_withNullIssueId_isNoOp() {
		TreeViewBrowserHandler handler = new TreeViewBrowserHandler(null);
		handler.selectNode(null);
	}

	@Test
	void selectNode_withEmptyIssueId_isNoOp() {
		TreeViewBrowserHandler handler = new TreeViewBrowserHandler(null);
		handler.selectNode("");
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
}
