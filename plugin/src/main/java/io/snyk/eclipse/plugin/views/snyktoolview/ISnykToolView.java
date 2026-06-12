package io.snyk.eclipse.plugin.views.snyktoolview;

/**
 * This interface captures the externally used methods with the tool window.
 * Having it, should allow for easier testing of the business logic apart from
 * UI.
 */
public interface ISnykToolView {
	/**
	 * Refreshes the delta reference labels
	 */
	abstract void refreshDeltaReference();

	/**
	 * Refreshes the browser
	 */
	abstract void refreshBrowser(String status);

	/**
	 * Refreshes the SummaryPanel
	 */
	abstract void updateSummary(String status);

	/**
	 * Hides or shows the Ignore buttons.
	 */
	abstract void toggleIgnoresButtons();

	abstract void selectTreeNode(String issueId, String product);

	abstract void updateTreeViewHtml(String html);
}
