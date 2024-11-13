package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.TreeNode;

public class RootObject {
	private List<TreeNode> children;

	public RootObject() {
		children = new ArrayList<>();

		// Initialize with some data
		children.add(new TreeNode("Open Source"));
		children.add(new TreeNode("Code Security"));
		children.add(new TreeNode("Configuration"));
	}

	public List<TreeNode> getChildren() {
		return children;
	}
}