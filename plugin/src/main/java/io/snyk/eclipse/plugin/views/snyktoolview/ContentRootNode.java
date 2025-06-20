package io.snyk.eclipse.plugin.views.snyktoolview;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_OSS;

import java.nio.file.Path;

import org.eclipse.jface.resource.ImageDescriptor;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.utils.ResourceUtils;

public final class ContentRootNode extends BaseTreeNode {
	private Path path;
	private String name;
	private String label;

	public ContentRootNode(String name, String label, Path value) {
		super(value);
		this.reset();
		this.setName(name);
		this.setLabel(label);
		this.setPath(value);
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		if (path == null) return null;
		var iResource = ResourceUtils.getProjectByPath(path);
		return getImageDescriptor(iResource);
	}

	public ProductTreeNode getProductNode(String product) {
		if (product == null) {
			return null;
		}
		switch (product) {
		case ProductConstants.DISPLAYED_OSS:
			return (ProductTreeNode) this.getChildren()[0];
		case ProductConstants.DISPLAYED_CODE_SECURITY:
			return (ProductTreeNode) this.getChildren()[1];
		case ProductConstants.DISPLAYED_IAC:
			return (ProductTreeNode) this.getChildren()[2];
		default:
			throw new IllegalArgumentException("unknown product in tree");
		}
	}
	
	@Override
	public void reset() {
		var ossRootNode = new ProductTreeNode(DISPLAYED_OSS);
		ossRootNode.setParent(this);

		var codeSecurityRootNode = new ProductTreeNode(DISPLAYED_CODE_SECURITY);
		codeSecurityRootNode.setParent(this);

		var iacRootNode = new ProductTreeNode(DISPLAYED_IAC);
		iacRootNode.setParent(this);

		ProductTreeNode[] productNodes = new ProductTreeNode[] { ossRootNode, codeSecurityRootNode, iacRootNode, };
		this.setChildren(productNodes);
	}

	@Override
	public String getText() {
		return getName() + getLabel();
	}

	public Path getPath() {
		return path;
	}

	public final void setPath(Path path) {
		if (path != null) {
			this.path = path.normalize();
		}
	}

	public String getName() {
		return name;
	}

	public String getLabel() {
		return label;
	}


	public final void setName(String name) {
		this.name = name;
	}

	public final void setLabel(String label) {
		this.label = label;
	}
}
