package io.snyk.eclipse.plugin.views.snyktoolview;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_QUALITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_OSS;

public class RootNode extends BaseTreeNode {
	private BaseTreeNode ossRootNode;
	private BaseTreeNode codeSecurityRootNode;
	private BaseTreeNode codeQualityRootNode;
	private BaseTreeNode iacRootNode;

	public RootNode() {
		super("");

		ossRootNode = new ProductTreeNode(DISPLAYED_OSS);
		codeSecurityRootNode = new ProductTreeNode(DISPLAYED_CODE_SECURITY);
		codeQualityRootNode = new ProductTreeNode(DISPLAYED_CODE_QUALITY);
		iacRootNode = new ProductTreeNode(DISPLAYED_IAC);

		BaseTreeNode[] children = new BaseTreeNode[] { ossRootNode, codeSecurityRootNode, codeQualityRootNode,
				iacRootNode, };
		setChildren(children);
	}
}
