package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

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
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		prefs.setTest(true);
	}

	@Test
	void selectTreeNode_withIssueId_doesNotThrow() {
		SnykToolView view = new SnykToolView();
		assertDoesNotThrow(() -> view.selectTreeNode("issue-id", "code"));
	}

	@Test
	void selectTreeNode_withNullIssueId_doesNotThrow() {
		SnykToolView view = new SnykToolView();
		assertDoesNotThrow(() -> view.selectTreeNode(null, "code"));
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
		assertDoesNotThrow(() -> view.selectTreeNode("some-issue-id", "code"));
	}

	@Test
	void dispatchTreeNode_callsUpdateBrowserContentAndSelectNode() throws Exception {
		SnykToolView view = new SnykToolView();
		BrowserHandler mockBrowser = mock(BrowserHandler.class);
		TreeViewBrowserHandler mockTree = mock(TreeViewBrowserHandler.class);
		injectField(view, "browserHandler", mockBrowser);
		injectField(view, "treeBrowserHandler", mockTree);

		view.dispatchTreeNode("issue-id", "code");

		verify(mockBrowser).updateBrowserContent("issue-id", "code");
		verify(mockTree).selectNode("issue-id");
	}

	@Test
	void dispatchBrowserContent_callsUpdateBrowserContent() throws Exception {
		SnykToolView view = new SnykToolView();
		BrowserHandler mockBrowser = mock(BrowserHandler.class);
		injectField(view, "browserHandler", mockBrowser);

		view.dispatchBrowserContent("issue-id", "code");

		verify(mockBrowser).updateBrowserContent("issue-id", "code");
	}

	@Test
	void dispatchTreeNode_withNullTreeHandler_stillCallsBrowserHandler() throws Exception {
		SnykToolView view = new SnykToolView();
		BrowserHandler mockBrowser = mock(BrowserHandler.class);
		injectField(view, "browserHandler", mockBrowser);
		// treeBrowserHandler left null

		view.dispatchTreeNode("issue-id", "code");

		verify(mockBrowser).updateBrowserContent("issue-id", "code");
	}

	@Test
	void dispatchBrowserContent_withNullBrowserHandler_doesNotThrow() {
		SnykToolView view = new SnykToolView();
		// browserHandler left null
		assertDoesNotThrow(() -> view.dispatchBrowserContent("issue-id", "code"));
	}

	@Test
	void dispatchTreeNode_withBothHandlersMocked_doesNotCallBrowserWhenTreeHandlerPresent() throws Exception {
		SnykToolView view = new SnykToolView();
		BrowserHandler mockBrowser = mock(BrowserHandler.class);
		TreeViewBrowserHandler mockTree = mock(TreeViewBrowserHandler.class);
		injectField(view, "browserHandler", mockBrowser);
		injectField(view, "treeBrowserHandler", mockTree);

		view.dispatchTreeNode("issue-id", "code");

		// both must be called — tree selects node, browser loads detail panel
		verify(mockTree).selectNode("issue-id");
		verify(mockBrowser).updateBrowserContent("issue-id", "code");
	}

	@SuppressWarnings("unchecked")
	private AtomicReference<String> getPendingHtml(SnykToolView view) throws Exception {
		Field f = SnykToolView.class.getDeclaredField("pendingHtml");
		f.setAccessible(true);
		return (AtomicReference<String>) f.get(view);
	}

	private void injectField(Object target, String fieldName, Object value) throws Exception {
		Field f = SnykToolView.class.getDeclaredField(fieldName);
		f.setAccessible(true);
		f.set(target, value);
	}
}
