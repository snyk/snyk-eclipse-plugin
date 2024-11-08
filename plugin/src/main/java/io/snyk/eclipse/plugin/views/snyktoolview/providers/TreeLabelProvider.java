package io.snyk.eclipse.plugin.views.snyktoolview.providers;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.swt.graphics.Image;

//TODO move this provider into a provider package
public class TreeLabelProvider implements ILabelProvider {

	@Override
	public String getText(Object element) {
		// Return the text to display for each tree item
		if (element instanceof TreeNode) {
			return ((TreeNode) element).getValue().toString();
		}
		return element.toString();
	}

	@Override
	public Image getImage(Object element) {
		// Return an image for each tree item (optional)
		// You can return null if you don't want to display images
		return null;
	}

	@Override
	public void addListener(ILabelProviderListener listener) {
		// Add a listener if needed
	}

	@Override
	public void dispose() {
		// Clean up resources if needed
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		// Return true if the label should be updated when the given property changes
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener listener) {
		// Remove a listener if needed
	}
}