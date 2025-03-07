package io.snyk.eclipse.plugin.utils;

import org.eclipse.jface.resource.ImageDescriptor;

import io.snyk.eclipse.plugin.Activator;

public class SnykIcons {
	public static final ImageDescriptor CODE = Activator.getImageDescriptor("CODE");
	public static final ImageDescriptor CODE_DISABLED = Activator.getImageDescriptor("CODE_DISABLED");

	public static final ImageDescriptor OSS = Activator.getImageDescriptor("OSS");
	public static final ImageDescriptor OSS_DISABLED = Activator.getImageDescriptor("OSS_DISABLED");

	public static final ImageDescriptor IAC = Activator.getImageDescriptor("IAC");
	public static final ImageDescriptor IAC_DISABLED = Activator.getImageDescriptor("IAC_DISABLED");

	public static final ImageDescriptor SEVERITY_CRITICAL = Activator.getImageDescriptor("SEVERITY_CRITICAL");
	public static final ImageDescriptor SEVERITY_HIGH = Activator.getImageDescriptor("SEVERITY_HIGH");
	public static final ImageDescriptor SEVERITY_MEDIUM = Activator.getImageDescriptor("SEVERITY_MEDIUM");
	public static final ImageDescriptor SEVERITY_LOW = Activator.getImageDescriptor("SEVERITY_LOW");

	public static final ImageDescriptor ENABLED = Activator.getImageDescriptor("ENABLED");
	public static final ImageDescriptor DISABLED = Activator.getImageDescriptor("DISABLED");
}
