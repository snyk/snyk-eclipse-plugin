package io.snyk.eclipse.plugin.views.snyktoolview;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_QUALITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_OSS;

import org.eclipse.jface.viewers.TreeNode;

public class RootNode extends TreeNode {
	private TreeNode ossRootNode;
	private TreeNode codeSecurityRootNode;
	private TreeNode codeQualityRootNode;
	private TreeNode iacRootNode;

	public RootNode() {
		super("");

		ossRootNode = new TreeNode(DISPLAYED_OSS);
		codeSecurityRootNode = new TreeNode(DISPLAYED_CODE_SECURITY);
		codeQualityRootNode = new TreeNode(DISPLAYED_CODE_QUALITY);
		iacRootNode = new TreeNode(DISPLAYED_IAC);

		TreeNode[] children = new TreeNode[] { ossRootNode, codeSecurityRootNode, codeQualityRootNode, iacRootNode, };
		setChildren(children);
	}
}
