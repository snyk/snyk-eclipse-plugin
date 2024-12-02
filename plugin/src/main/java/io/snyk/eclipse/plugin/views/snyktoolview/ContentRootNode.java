package io.snyk.eclipse.plugin.views.snyktoolview;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_QUALITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_OSS;

import java.nio.file.Path;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.model.WorkbenchLabelProvider;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.utils.ResourceUtils;

public class ContentRootNode extends BaseTreeNode {
	private Path path;
	private String name;

	public ContentRootNode(String name, Path value) {
		super(value);
		reset();
		this.name = name;
		this.setPath(value);
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		ILabelProvider labelProvider = WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider();
		try {
			var object = ResourceUtils.getProjectByPath(path);
			Image image = labelProvider.getImage(object);
			if (image == null || image.isDisposed())
				return null;

			return ImageDescriptor.createFromImage(image);
		} finally {
			labelProvider.dispose();
		}
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
		case ProductConstants.DISPLAYED_CODE_QUALITY:
			return (ProductTreeNode) this.getChildren()[2];
		case ProductConstants.DISPLAYED_IAC:
			return (ProductTreeNode) this.getChildren()[3];
		}
		return null;
	}

	@Override
	public void reset() {
		var ossRootNode = new ProductTreeNode(DISPLAYED_OSS);
		ossRootNode.setParent(this);

		var codeSecurityRootNode = new ProductTreeNode(DISPLAYED_CODE_SECURITY);
		codeSecurityRootNode.setParent(this);

		var codeQualityRootNode = new ProductTreeNode(DISPLAYED_CODE_QUALITY);
		codeQualityRootNode.setParent(this);

		var iacRootNode = new ProductTreeNode(DISPLAYED_IAC);
		iacRootNode.setParent(this);

		ProductTreeNode[] productNodes = new ProductTreeNode[] { ossRootNode, codeSecurityRootNode, codeQualityRootNode,
				iacRootNode, };
		this.setChildren(productNodes);
	}

	@Override
	public String getText() {
		return name;
	}

	public Path getPath() {
		return path;
	}

	public void setPath(Path path) {
		if (!path.toString().endsWith(this.name)) {
			throw new IllegalArgumentException(value + " does not end with " + name);
		}
		this.path = path.normalize();
	}
}
