package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.jface.viewers.TreeNode;

public class RootObject extends TreeNode {
	public static final String CONFIGURATION = "Configuration";
	public static final String CODE_SECURITY = "Code Security";
	public static final String OPEN_SOURCE = "Open Source";
	public static final String CODE_QUALITY = "Code Quality";
	private TreeNode ossRootNode;
	private TreeNode codeSecurityRootNode;
	private TreeNode codeQualityRootNode;
	private TreeNode iacRootNode;

	public RootObject() {
		super("");
		
		ossRootNode = new TreeNode(OPEN_SOURCE);
		codeSecurityRootNode = new TreeNode(CODE_SECURITY);
		codeQualityRootNode = new TreeNode(CODE_QUALITY);
		iacRootNode = new TreeNode(CONFIGURATION);
		
		TreeNode[] children = new TreeNode[] {
			ossRootNode,
			codeSecurityRootNode,
			codeQualityRootNode,
			iacRootNode,
		};
		setChildren(children);
	}
}