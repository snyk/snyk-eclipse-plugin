package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.jface.resource.ImageDescriptor;

/**
 * This interface captures the externally used methods with the tool window.
 * Having it, should allow for easier testing of the business logic apart from
 * UI.
 */
public interface ISnykToolView {
	String CONGRATS_NO_ISSUES_FOUND = "✅ Congrats! No issues found!";
	String NO_FIXABLE_ISSUES = "There are no issues automatically fixable.";
	String IGNORED_ISSUES_FILTERED_BUT_AVAILABLE = "Adjust your Issue View Options to see ignored issues.";
	String OPEN_ISSUES_FILTERED_BUT_AVAILABLE = "Adjust your Issue View Options to open issues.";

	/**
	 * Updates the text of the given node
	 * 
	 * @param node
	 * @param text
	 */
	abstract void setNodeText(BaseTreeNode node, String text);

	/**
	 * Sets the icon of the given node
	 * 
	 * @param icon
	 */
	abstract void setNodeIcon(ImageDescriptor icon);

	/**
	 * Adds an issue node to the parent (usually a file node)
	 * 
	 * @param parent
	 * @param toBeAdded
	 */
	abstract void addIssueNode(BaseTreeNode parent, BaseTreeNode toBeAdded);

	/**
	 * Adds a file node (usually below the product node)
	 * 
	 * @param parent
	 * @param toBeAdded
	 */
	abstract void addFileNode(BaseTreeNode parent, BaseTreeNode toBeAdded);

	/**
	 * Adds an info node (usually below the product node)
	 * 
	 * @param parent
	 * @param toBeAdded
	 */
	abstract void addInfoNode(BaseTreeNode parent, BaseTreeNode toBeAdded);

	/**
	 * Returns the product node
	 * 
	 * @param product the product. ProductConstants#DISPLAY_*
	 * @return
	 */
	abstract BaseTreeNode getProductNode(String product);

	/**
	 * Returns the tree root
	 * 
	 * @return
	 */
	abstract BaseTreeNode getRoot();

	static String getPlural(long count) {
		return count > 1 ? "s" : "";
	}
}
