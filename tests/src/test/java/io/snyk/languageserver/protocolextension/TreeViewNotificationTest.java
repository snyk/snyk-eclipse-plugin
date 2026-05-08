package io.snyk.languageserver.protocolextension;

import static org.mockito.Mockito.after;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.protocolextension.messageObjects.TreeViewParams;

class TreeViewNotificationTest extends LsBaseTest {

	private SnykExtendedLanguageClient cut;
	private ISnykToolView toolWindowMock;

	@BeforeEach
	protected void setUp() {
		super.setUp();
		Preferences.getInstance().store(Preferences.AUTH_TOKEN_KEY, "dummy");
		Preferences.getInstance().store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "false");
		toolWindowMock = mock(ISnykToolView.class);
	}

	@AfterEach
	protected void tearDown() {
		reset(toolWindowMock);
		super.tearDown();
	}

	// T-S-001: TreeViewParams DTO carries treeViewHtml
	@Test
	void testTreeViewParamsHoldsHtml() {
		TreeViewParams params = new TreeViewParams();
		params.setTreeViewHtml("<html>test</html>");

		org.junit.jupiter.api.Assertions.assertEquals("<html>test</html>", params.getTreeViewHtml());
	}

	// T-I-001: snykTreeView notification dispatches html to ISnykToolView.updateTreeViewHtml
	@Test
	void testSnykTreeViewDispatchesHtmlToToolView() {
		Preferences.getInstance().store(Preferences.USE_HTML_TREE_VIEW, "true");
		TreeViewParams params = new TreeViewParams();
		params.setTreeViewHtml("<html>issues</html>");

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykTreeView(params);

		verify(toolWindowMock, timeout(15000).times(1)).updateTreeViewHtml("<html>issues</html>");
	}

	// Null params must not throw or call toolView
	@Test
	void testSnykTreeViewWithNullParamsIsNoOp() {
		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykTreeView(null);

		verify(toolWindowMock, after(500).never()).updateTreeViewHtml(any());
	}

	// Null html in params must not call toolView
	@Test
	void testSnykTreeViewWithNullHtmlIsNoOp() {
		Preferences.getInstance().store(Preferences.USE_HTML_TREE_VIEW, "true");
		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykTreeView(new TreeViewParams());

		verify(toolWindowMock, after(500).never()).updateTreeViewHtml(any());
	}

	// Preference disabled: notification must be silently dropped
	@Test
	void testSnykTreeViewSkipsWhenPreferenceDisabled() {
		Preferences.getInstance().store(Preferences.USE_HTML_TREE_VIEW, "false");
		TreeViewParams params = new TreeViewParams();
		params.setTreeViewHtml("<html>issues</html>");

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykTreeView(params);

		verify(toolWindowMock, after(500).never()).updateTreeViewHtml(any());
	}
}
