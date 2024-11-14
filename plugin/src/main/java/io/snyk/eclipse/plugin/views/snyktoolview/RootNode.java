package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.jface.viewers.TreeNode;

import io.snyk.eclipse.plugin.domain.ProductConstants;

public class RootNode extends TreeNode {
	private TreeNode ossRootNode;
	private TreeNode codeSecurityRootNode;
	private TreeNode codeQualityRootNode;
	private TreeNode iacRootNode;

	public RootNode() {
		super("");

		ossRootNode = new TreeNode(ProductConstants.OPEN_SOURCE);
		codeSecurityRootNode = new TreeNode(ProductConstants.CODE_SECURITY);
		codeQualityRootNode = new TreeNode(ProductConstants.CODE_QUALITY);
		iacRootNode = new TreeNode(ProductConstants.CONFIGURATION);

		TreeNode[] children = new TreeNode[] { ossRootNode, codeSecurityRootNode, codeQualityRootNode, iacRootNode, };
		setChildren(children);
	}
}