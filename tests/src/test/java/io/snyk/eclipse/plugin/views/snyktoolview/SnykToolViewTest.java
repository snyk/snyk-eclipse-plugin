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

class SnykToolViewTest {

	@BeforeEach
	void setUp() {
		Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
	}

	@Test
	void selectTreeNode_withIssueId_doesNotThrow() {
		SnykToolView view = new SnykToolView();
		assertDoesNotThrow(() -> view.selectTreeNode("issue-id"));
	}

	@Test
	void selectTreeNode_withNullIssueId_doesNotThrow() {
		SnykToolView view = new SnykToolView();
		assertDoesNotThrow(() -> view.selectTreeNode(null));
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

	@Test
	void selectTreeNode_withNullHandler_doesNotThrow() {
		SnykToolView view = new SnykToolView();
		assertDoesNotThrow(() -> view.selectTreeNode("some-issue-id"));
	}

	@SuppressWarnings("unchecked")
	private AtomicReference<String> getPendingHtml(SnykToolView view) throws Exception {
		Field f = SnykToolView.class.getDeclaredField("pendingHtml");
		f.setAccessible(true);
		return (AtomicReference<String>) f.get(view);
	}
}
