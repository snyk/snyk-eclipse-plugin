package io.snyk.eclipse.plugin.views.snyktoolview.providers;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.swt.graphics.Image;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.domain.ProductConstants;

public class TreeLabelProvider implements ILabelProvider {

	public static final ImageDescriptor OSS = Activator.getImageDescriptor("/icons/oss.png");
	public static final ImageDescriptor CodeSecurity = Activator.getImageDescriptor("/icons/code.png");
	public static final ImageDescriptor CodeQuality = Activator.getImageDescriptor("/icons/code.png");
	public static final ImageDescriptor IAC = Activator.getImageDescriptor("/icons/iac.png");

	private Image ossImage;
	private Image codeSecurityImage;
	private Image configurationImage;
	private Image codeQualityImage;

	public TreeLabelProvider() {
		ossImage = OSS.createImage();
		codeSecurityImage = CodeSecurity.createImage();
		codeQualityImage = CodeQuality.createImage();
		configurationImage = IAC.createImage();
	}

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
		if (element instanceof TreeNode) {
			String elementNode = ((TreeNode) element).getValue().toString();
			switch (elementNode) {
			case ProductConstants.OPEN_SOURCE:
				return ossImage;
			case ProductConstants.CODE_SECURITY:
				return codeSecurityImage;
			case ProductConstants.CODE_QUALITY:
				return codeQualityImage;
			case ProductConstants.CONFIGURATION:
				return configurationImage;
			default:
				return null; 
			}
		}
		return null;
	}

	@Override
	public void addListener(ILabelProviderListener listener) {
		// Add a listener if needed
	}

	@Override
	public void dispose() {
		if (ossImage != null && !ossImage.isDisposed()) {
			ossImage.dispose();
			ossImage = null;
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