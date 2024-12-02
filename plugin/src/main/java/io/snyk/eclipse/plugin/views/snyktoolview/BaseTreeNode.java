package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeNode;

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
		this.setChildren(list.toArray(new BaseTreeNode[list.size()]));
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
		this.value = null;
		this.imageDescriptor = null;
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
