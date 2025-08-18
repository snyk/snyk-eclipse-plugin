package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

public class BaseTreeNode extends TreeNode {
	private ImageDescriptor imageDescriptor;
	private String text = "";

	public BaseTreeNode(Object value) {
		super(value);
		if (value instanceof String)
			this.text = value.toString();
	}

	public void setValue(Object value) {
		this.value = value;
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

	public final void setImageDescriptor(ImageDescriptor imageDescriptor) {
		this.imageDescriptor = imageDescriptor;
	}
	
	protected ImageDescriptor getImageDescriptor(IResource object) {	
		// Get the image descriptor directly from the workbench to avoid disposed image issues
		try {
			imageDescriptor = PlatformUI.getWorkbench().getEditorRegistry()
				.getImageDescriptor(object.getName());
			
			// If no descriptor found for file type, try to get default file icon
			if (imageDescriptor == null) {
				imageDescriptor = PlatformUI.getWorkbench().getSharedImages()
					.getImageDescriptor(ISharedImages.IMG_OBJ_FILE);
			}
			
			return imageDescriptor;
		} catch (Exception e) {
			// Return null if workbench is not available or any error occurs
			return null;
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

	@Override
	public String toString() {
		return this.value.toString();
	}

	public void reset() {
		this.removeChildren();
		this.text = "";
		this.value = null; // NOPMD by bdoetsch on 3/12/25, 10:27 AM
		this.imageDescriptor = null; // NOPMD by bdoetsch on 3/12/25, 10:27 AM
	}

	/**
	 * Provides the details to be displayed in the details view
	 * 
	 * @return html details
	 */
	public String getDetails() {
		return "";
	}
}
