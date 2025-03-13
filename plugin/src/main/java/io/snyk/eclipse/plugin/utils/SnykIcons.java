package io.snyk.eclipse.plugin.utils;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;

import io.snyk.eclipse.plugin.Activator;

public class SnykIcons {
	public static final String CODE_ID = "CODE";
	public static final String CODE_DISABLED_ID = "CODE_DISABLED";
	public static final String OSS_ID = "OSS";
	public static final String OSS_DISABLED_ID = "OSS_DISABLED";
	public static final String IAC_ID = "IAC";
	public static final String IAC_DISABLED_ID = "IAC_DISABLED";
	public static final String SEVERITY_CRITICAL_ID = "SEVERITY_CRITICAL";
	public static final String SEVERITY_HIGH_ID = "SEVERITY_HIGH";
	public static final String SEVERITY_MEDIUM_ID = "SEVERITY_MEDIUM";
	public static final String SEVERITY_LOW_ID = "SEVERITY_LOW";
	public static final String ENABLED_ID = "ENABLED";
	public static final String DISABLED_ID = "DISABLED";

	public static final ImageDescriptor getImageDescriptor(String descriptorId) {
		return Activator.getDefault().getImageDescriptor(descriptorId);
	}

	public static final void addImageDescriptorToRegistry(String id, ImageDescriptor imageDescriptor) {
		Display.getDefault().syncExec(() -> {
			final var imageRegistry = Activator.getDefault().getImageRegistry();
			imageRegistry.put(id, imageDescriptor);
		});

	}
}
