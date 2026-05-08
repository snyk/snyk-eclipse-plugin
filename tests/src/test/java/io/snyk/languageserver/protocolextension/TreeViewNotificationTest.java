package io.snyk.languageserver.protocolextension;

import static org.mockito.Mockito.mock;
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

	// T-S-001: TreeViewParams DTO carries treeViewHtml and totalIssues
	@Test
	void testTreeViewParamsHoldsHtmlAndTotalIssues() {
		TreeViewParams params = new TreeViewParams();
		params.setTreeViewHtml("<html>test</html>");
		params.setTotalIssues(5);

		org.junit.jupiter.api.Assertions.assertEquals("<html>test</html>", params.getTreeViewHtml());
		org.junit.jupiter.api.Assertions.assertEquals(5, params.getTotalIssues());
	}

	// T-I-001: snykTreeView notification dispatches html to ISnykToolView.updateTreeViewHtml
	@Test
	void testSnykTreeViewDispatchesHtmlToToolView() {
		TreeViewParams params = new TreeViewParams();
		params.setTreeViewHtml("<html>issues</html>");
		params.setTotalIssues(3);

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykTreeView(params);

		verify(toolWindowMock, timeout(5000).times(1)).updateTreeViewHtml("<html>issues</html>");
	}
}
