package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeNode;

public interface ISnykToolView {
	abstract void setNodeText(TreeNode node, String text);

	abstract void setNodeIcon(ImageDescriptor icon);

	abstract void addIssueNode(TreeNode parent, TreeNode toBeAdded);

	abstract void addFileNode(TreeNode parent, TreeNode toBeAdded);

	abstract void addInfoNode(TreeNode parent, TreeNode toBeAdded);

	abstract TreeNode getProductNode(String product);

	abstract TreeNode getRoot();
}
