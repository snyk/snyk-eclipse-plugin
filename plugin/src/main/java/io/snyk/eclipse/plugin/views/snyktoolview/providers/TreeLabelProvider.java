package io.snyk.eclipse.plugin.views.snyktoolview.providers;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

import io.snyk.eclipse.plugin.utils.SnykLogger;
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
				// Remove disposed image from cache to prevent memory leak
				if (image != null && image.isDisposed()) {
					images.remove(imageDescriptor);
				}
				// Create new image and cache it
				image = (Image) imageDescriptor.createResource(Display.getDefault());
				if (image != null) {
					images.put(imageDescriptor, image);
				}
			}
			return image;
		}
	}

	@Override
	public void addListener(ILabelProviderListener listener) {
		// Add a listener if needed
	}

	@Override
	public void dispose() {
		synchronized (images) {
			for (var entry : images.entrySet()) {
				try {
					Image image = entry.getValue();
					if (image != null && !image.isDisposed()) {
						entry.getKey().destroyResource(image);
					}
				} catch (Exception e) {
					// Log but continue disposing other images
					SnykLogger.logError(e);
				}
			}
			images.clear();
		}
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