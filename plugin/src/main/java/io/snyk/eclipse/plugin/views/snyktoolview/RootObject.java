package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.TreeItem;
import io.snyk.eclipse.plugin.Activator;

public class RootObject {
	private List<TreeNode> children;

	public RootObject() {
		children = new ArrayList<>();
		// Initialize with some data
		TreeNode node = new TreeNode("Open Source");
		children.add(node);
		children.add(new TreeNode("Code Security"));
		children.add(new TreeNode("Configuration"));
	}

	public List<TreeNode> getChildren() {
		return children;
	}
}