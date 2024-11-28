package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.jface.viewers.TreeViewer;

/**
 * This interface captures the externally used methods with the tool window.
 * Having it, should allow for easier testing of the business logic apart from
 * UI.
 */
public interface ISnykToolView {
	String CONGRATS_NO_ISSUES_FOUND = "✅ Congrats! No issues found!";
	String NO_FIXABLE_ISSUES = "There are no issues automatically fixable.";
	String IGNORED_ISSUES_FILTERED_BUT_AVAILABLE = "Adjust your Issue View Options to see ignored issues.";
	String OPEN_ISSUES_FILTERED_BUT_AVAILABLE = "Adjust your Issue View Options to see open issues.";

	String NODE_TEXT_SCANNING = "Scanning...";
	String NODE_TEXT_NO_ISSUES_FOUND = "No issues found";
	String NODE_TEXT_ERROR = "An error occurred";

	/**
	 * Updates the text of the given node
	 * 
	 * @param node
	 * @param text
	 */
	abstract void setNodeText(BaseTreeNode node, String text);

	/**
	 * Adds an issue node to the parent (usually a file node)
	 * 
	 * @param parent
	 * @param toBeAdded
	 */
	abstract void addIssueNode(FileTreeNode parent, IssueTreeNode toBeAdded);

	/**
	 * Adds a file node (usually below the product node)
	 * 
	 * @param parent
	 * @param toBeAdded
	 */
	abstract void addFileNode(ProductTreeNode parent, FileTreeNode toBeAdded);

	/**
	 * Adds an info node (usually below the product node)
	 * 
	 * @param parent
	 * @param toBeAdded
	 */
	abstract void addInfoNode(ProductTreeNode parent, InfoTreeNode toBeAdded);

	/**
	 * Returns the product node
	 * 
	 * @param product    the product. ProductConstants#DISPLAY_
	 * @param folderPath TODO*
	 * @return
	 */
	abstract ProductTreeNode getProductNode(String product, String folderPath);

	/**
	 * Resets a product node
	 */
	abstract void resetNode(BaseTreeNode node);

	/**
	 * Remove all info nodes from the product tree node
	 * 
	 * @param node
	 */
	abstract void removeInfoNodes(ProductTreeNode node);

	/**
	 * Refreshes the tree display
	 */
	abstract void refreshTree();

	/**
	 * Returns the tree root
	 * 
	 * @return
	 */
	abstract BaseTreeNode getRoot();

	/**
	 * Clears all nodes in the tree
	 * 
	 * @return
	 */
	abstract void clearTree();

	static String getPlural(long count) {
		return count > 1 ? "s" : "";
	}

	abstract TreeViewer getTreeViewer();

	/**
	 * Hides or shows the Ignore buttons.
	 * 
	 * @return
	 */
	abstract void toggleIgnoresButtons();

	/**
	 * Enables the net new issues scans.
	 * 
	 * @return
	 */
	abstract void enableDelta();

	/**
	 * Disable the net new issues scans.
	 * 
	 * @return
	 */
	abstract void disableDelta();

	/**
	 * Remove the scan results for the Project from the TreeViewer.
	 *
	 * @param project
	 */
	abstract void resetContentRootNode(String project);
}
