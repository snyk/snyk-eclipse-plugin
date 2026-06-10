package io.snyk.eclipse.plugin.views.snyktoolview;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

/**
 * This interface captures the externally used methods with the tool window.
 * Having it, should allow for easier testing of the business logic apart from
 * UI.
 */
public interface ISnykToolView {
	public String CONGRATS_NO_ISSUES_FOUND = "✅ Congrats! No issues found!";
	public String CONGRATS_NO_OPEN_ISSUES_FOUND = "✅ Congrats! No open issues found!";
	public String OPEN_ISSUES_ARE_DISABLED = "Open issues are disabled!";
	public String NO_IGNORED_ISSUES = "✋ No ignored issues, open issues are disabled";
	public String OPEN_AND_IGNORED_ISSUES_ARE_DISABLED = "Open and Ignored issues are disabled!";
	public String NO_FIXABLE_ISSUES = "There are no issues automatically fixable.";
	public String IGNORED_ISSUES_FILTERED_BUT_AVAILABLE = "Adjust your settings to view Ignored issues.";
	public String OPEN_ISSUES_FILTERED_BUT_AVAILABLE = "Adjust your settings to view Open issues.";
	public String ALL_ISSUES_FILTERED_BUT_AVAILABLE = "Adjust your settings to view Open or Ignored issues.";

	public String NODE_TEXT_SCANNING = "Scanning...";
	public String NODE_TEXT_ERROR = "(error)";

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

	/**
	 * Enables the net new issues scans.
	 */
	abstract void enableDelta();

	/**
	 * Disable the net new issues scans.
	 */
	abstract void disableDelta();

	abstract void selectTreeNode(Issue issue, String product);

	abstract void updateTreeViewHtml(String html);
}
