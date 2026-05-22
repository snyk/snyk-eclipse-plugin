package io.snyk.eclipse.plugin.views.snyktoolview;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

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
}
