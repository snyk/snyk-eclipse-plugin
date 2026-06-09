package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicReference;

import org.eclipse.jface.viewers.TreeViewer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.preferences.InMemorySecurePreferenceStore;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

class SnykToolViewTest {

	@BeforeEach
	void setUp() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		prefs.store(Preferences.USE_HTML_TREE_VIEW, "false");
	}

	@Test
	void selectTreeNode_withUnknownProduct_doesNotThrow() {
		SnykToolView view = new SnykToolView() {
			@Override
			public ProductTreeNode getProductNode(String product, String folderPath) {
				return null;
			}
		};

		Issue issue = new Issue("issue-id", "Test Issue", "high", "/some/path/File.java",
				null, false, false, "Code Security", null, null);

		assertDoesNotThrow(() -> view.selectTreeNode(issue, "unknownProduct"));
	}

	@Test
	void selectTreeNode_withNullIssue_doesNotThrow() {
		SnykToolView view = new SnykToolView() {
			@Override
			public ProductTreeNode getProductNode(String product, String folderPath) {
				return null;
			}
		};

		assertDoesNotThrow(() -> view.selectTreeNode(null, "Snyk Open Source"));
	}

	// Regression: HTML arriving before createPartControl (treeBrowserHandler == null)
	// must be stored in pendingHtml so createPartControl can drain it on init.
	@Test
	void updateTreeViewHtml_whenHandlerNull_buffersHtmlInPendingHtml() throws Exception {
		SnykToolView view = new SnykToolView();

		view.updateTreeViewHtml("<html>buffered</html>");

		AtomicReference<?> pendingHtml = getPendingHtml(view);
		assertEquals("<html>buffered</html>", pendingHtml.get());
	}

	// Regression: subsequent calls overwrite with the latest HTML (last-write-wins).
	@Test
	void updateTreeViewHtml_multipleCallsWhenHandlerNull_keepsLatestHtml() throws Exception {
		SnykToolView view = new SnykToolView();

		view.updateTreeViewHtml("<html>first</html>");
		view.updateTreeViewHtml("<html>second</html>");

		AtomicReference<?> pendingHtml = getPendingHtml(view);
		assertEquals("<html>second</html>", pendingHtml.get());
	}

	// Fix 4: HTML tree view enabled — selectTreeNode should not throw when handler is null
	@Test
	void selectTreeNode_withHtmlTreeViewEnabled_andNullHandler_doesNotThrow() {
		Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore())
				.store(Preferences.USE_HTML_TREE_VIEW, "true");
		SnykToolView view = new SnykToolView();
		Issue issue = new Issue("issue-id", "Test Issue", "high", "/some/path/File.java",
				null, false, false, "Code Security", null, null);
		assertDoesNotThrow(() -> view.selectTreeNode(issue, "Snyk Code"));
	}

	// Fix 4: HTML tree view enabled — selectTreeNode with null issue should not throw
	@Test
	void selectTreeNode_withHtmlTreeViewEnabled_andNullIssue_doesNotThrow() {
		Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore())
				.store(Preferences.USE_HTML_TREE_VIEW, "true");
		SnykToolView view = new SnykToolView();
		assertDoesNotThrow(() -> view.selectTreeNode(null, "Snyk Code"));
	}

	@Test
	void selectTreeNode_withHtmlTreeViewEnabled_updatesContentPanel() throws Exception {
		Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore())
				.store(Preferences.USE_HTML_TREE_VIEW, "true");

		Issue issue = new Issue("issue-id", "Test Issue", "high", "/some/path/File.java",
				null, false, false, "Code Security", null, null);

		// Build a product-like tree node without going through ProductTreeNode's
		// constructor (which calls SnykIcons and Activator — unavailable headlessly).
		// findIssueTreeNode accepts TreeNode, and the cast in selectTreeNode is
		// (TreeNode) productNode, so a BaseTreeNode subclass suffices.
		IssueTreeNode issueNode = new IssueTreeNode(issue);
		ProductTreeNode mockProductNode = mock(ProductTreeNode.class);
		when(mockProductNode.getChildren()).thenReturn(new IssueTreeNode[] { issueNode });

		BrowserHandler mockBrowserHandler = mock(BrowserHandler.class);
		TreeViewer mockTreeViewer = mock(TreeViewer.class);

		// Subclass overrides:
		// - getProductNode: returns our mock tree with the IssueTreeNode child
		// - runOnDisplay: runs the runnable synchronously so verify() sees the call
		SnykToolView view = new SnykToolView() {
			@Override
			public ProductTreeNode getProductNode(String product, String folderPath) {
				return mockProductNode;
			}

			@Override
			protected void runOnDisplay(Runnable runnable) {
				runnable.run();
			}
		};

		// Inject mock browserHandler via reflection.
		Field browserHandlerField = SnykToolView.class.getDeclaredField("browserHandler");
		browserHandlerField.setAccessible(true);
		browserHandlerField.set(view, mockBrowserHandler);

		// Inject mock treeViewer so the treeViewer == null guard does not short-circuit.
		Field treeViewerField = SnykToolView.class.getDeclaredField("treeViewer");
		treeViewerField.setAccessible(true);
		treeViewerField.set(view, mockTreeViewer);

		view.selectTreeNode(issue, "Snyk Code");

		verify(mockBrowserHandler).updateBrowserContent(issueNode);
	}

	@SuppressWarnings("unchecked")
	private AtomicReference<String> getPendingHtml(SnykToolView view) throws Exception {
		Field f = SnykToolView.class.getDeclaredField("pendingHtml");
		f.setAccessible(true);
		return (AtomicReference<String>) f.get(view);
	}
}
