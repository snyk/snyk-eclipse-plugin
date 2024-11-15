package io.snyk.eclipse.plugin.utils;

import org.eclipse.jface.resource.ImageDescriptor;

import io.snyk.eclipse.plugin.Activator;

public class ActivatorImageDescriptorProvider implements ImageDescriptorProvider {

	@Override
    public ImageDescriptor getImageDescriptor(String path) {
        return Activator.getImageDescriptor(path);
    }

}
