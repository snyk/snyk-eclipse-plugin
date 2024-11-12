package io.snyk.eclipse.plugin.views.snyktoolview.providers;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.jface.viewers.Viewer;

import io.snyk.eclipse.plugin.views.snyktoolview.RootObject;

public class TreeContentProvider implements ITreeContentProvider {
	@Override
	public Object[] getElements(Object inputElement) {
		// Return the root elements of your tree
		// This method is called to get the top-level items
		if (inputElement instanceof RootObject) {
			return ((RootObject) inputElement).getChildren().toArray();
		}
		return new Object[0];
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		// Return the children of the given element
		if (parentElement instanceof TreeNode) {
			return ((TreeNode) parentElement).getChildren();
		}
		return new Object[0];
	}

	@Override
	public Object getParent(Object element) {
		// Return the parent of the given element
		if (element instanceof TreeNode) {
			return ((TreeNode) element).getParent();
		}
		return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		// Return true if the element has children
		if (element instanceof TreeNode) {
			return ((TreeNode) element).hasChildren();
		}
		return false;
	}

	@Override
	public void dispose() {
		// Clean up any resources
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// Handle any changes when the input is set or changed
	}
}
