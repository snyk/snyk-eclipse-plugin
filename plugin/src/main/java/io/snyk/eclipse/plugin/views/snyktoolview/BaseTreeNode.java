package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageDataProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;

import io.snyk.eclipse.plugin.utils.SnykLogger;

public class BaseTreeNode extends TreeNode {
	private ImageDescriptor imageDescriptor;
	private String text = "";
	private Object value;

	public BaseTreeNode(Object value) {
		super(value);
		if (value instanceof String)
			this.text = value.toString();
	}

	public void addChild(BaseTreeNode child) {
		TreeNode[] children = getChildren();
		List<BaseTreeNode> list = new ArrayList<BaseTreeNode>();
		if (children != null) {
			var previousList = Arrays.asList(children).stream().map(it -> (BaseTreeNode) it)
					.collect(Collectors.toList());

			list = new ArrayList<BaseTreeNode>(previousList);
		}
		child.setParent(this);
		list.add(child);

		// Calls to a collection's `toArray(E[])` method should specify a target array
		// of zero size. This allows the JVM
		// to optimize the memory allocation and copying as much as possible.
		// https://shipilev.net/blog/2016/arrays-wisdom-ancients/
		this.setChildren(list.toArray(new BaseTreeNode[0]));
	}

	public void removeChildren() {
		TreeNode[] children = this.getChildren();
		if (children == null) {
			return;
		}

		for (TreeNode child : children) {
			child.setParent(null);
		}

		setChildren(new BaseTreeNode[0]);
	}

	public void setImageDescriptor(ImageDescriptor imageDescriptor) {
		this.imageDescriptor = imageDescriptor;
	}

	protected ImageDescriptor getImageDescriptor(IResource object) {
		ILabelProvider labelProvider = null;
		Image image = null;

		try {
			labelProvider = WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider();
			image = labelProvider.getImage(object);

			if (image == null || image.isDisposed()) {
				return null;
			}

			return getImageDescriptorFromImage(image);

		} catch (Exception e) {
			SnykLogger.logError(e);
			return null;
		} finally {
			disposeResources(labelProvider, image);
		}
	}

	private void disposeResources(ILabelProvider labelProvider, Image image) {
		if (image != null && !image.isDisposed()) {
			image.dispose();
		}
		if (labelProvider != null) {
			labelProvider.dispose();
		}
	}

	public ImageDescriptor getImageDescriptor() {
		return this.imageDescriptor;
	}

	public String getText() {
		return this.text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public void setValue(Object value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return this.value.toString();
	}

	public void reset() {
		this.removeChildren();
		this.text = "";
	}

	/**
	 * Provides the details to be displayed in the details view
	 * 
	 * @return html details
	 */
	public String getDetails() {
		return "";
	}

	protected ImageDescriptor getImageDescriptorFromImage(Image image) {
		final var data = image.getImageData();

		ImageDataProvider provider = new ImageDataProvider() {
			@Override
			public ImageData getImageData(int zoom) {
				return data;
			}
		};
		return ImageDescriptor.createFromImageDataProvider(provider);
	}
}
