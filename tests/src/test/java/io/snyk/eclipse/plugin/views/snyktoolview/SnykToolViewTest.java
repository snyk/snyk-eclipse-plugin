package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicReference;

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

	@SuppressWarnings("unchecked")
	private AtomicReference<String> getPendingHtml(SnykToolView view) throws Exception {
		Field f = SnykToolView.class.getDeclaredField("pendingHtml");
		f.setAccessible(true);
		return (AtomicReference<String>) f.get(view);
	}
}
