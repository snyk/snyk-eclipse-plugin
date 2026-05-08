package io.snyk.eclipse.plugin.views.snyktoolview;

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
