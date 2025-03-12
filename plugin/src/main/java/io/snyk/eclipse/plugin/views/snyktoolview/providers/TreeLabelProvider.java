package io.snyk.eclipse.plugin.views.snyktoolview.providers;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

import io.snyk.eclipse.plugin.views.snyktoolview.BaseTreeNode;

public class TreeLabelProvider implements ILabelProvider {
	private Map<ImageDescriptor, Image> images = new ConcurrentHashMap<ImageDescriptor, Image>();

	public TreeLabelProvider() {
	}

	@Override
	public String getText(Object element) {
		// Return the text to display for each tree item
		if (element instanceof BaseTreeNode) {
			return ((BaseTreeNode) element).getText();
		}
		return element.toString();
	}

	@Override
	public Image getImage(Object element) {
		if (!(element instanceof BaseTreeNode)) {
			return null;
		}

		var node = (BaseTreeNode) element;
		ImageDescriptor imageDescriptor = node.getImageDescriptor();

		if (imageDescriptor == null) {
			return null;
		}

		synchronized (images) {
			Image image = images.get(imageDescriptor);
			if (image == null || image.isDisposed()) {
				final var resource = imageDescriptor.createResource(Display.getDefault());
				images.put(imageDescriptor, (Image) resource);
			}
			return images.get(imageDescriptor);
		}
	}

	@Override
	public void addListener(ILabelProviderListener listener) {
		// Add a listener if needed
	}

	@Override
	public void dispose() {
		for (var entry : images.entrySet()) {
			entry.getKey().destroyResource(entry.getValue());
		}
		images.clear();
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