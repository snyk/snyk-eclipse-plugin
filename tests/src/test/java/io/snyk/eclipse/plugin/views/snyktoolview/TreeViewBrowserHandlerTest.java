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

	// Issue #4: U+2028 and U+2029 must be replaced with \u2028/\u2029 escape sequences.
	// The implementation must NOT use literal Unicode chars (which can be corrupted by tools).
	@Test
	void escapeJsSingleQuotedString_escapesLineSeparator_U2028() {
		String input = "before\u2028after";
		String escaped = TreeViewBrowserHandler.escapeJsSingleQuotedString(input);
		assertFalse(escaped.contains("\u2028"), "escaped string must not contain literal U+2028");
		assertTrue(escaped.contains("\\u2028"), "escaped string must contain \\u2028 escape");
	}

	@Test
	void escapeJsSingleQuotedString_escapesParagraphSeparator_U2029() {
		String input = "before\u2029after";
		String escaped = TreeViewBrowserHandler.escapeJsSingleQuotedString(input);
		assertFalse(escaped.contains("\u2029"), "escaped string must not contain literal U+2029");
		assertTrue(escaped.contains("\\u2029"), "escaped string must contain \\u2029 escape");
	}
}
