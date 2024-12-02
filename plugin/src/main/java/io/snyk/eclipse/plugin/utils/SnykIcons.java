package io.snyk.eclipse.plugin.utils;

import org.eclipse.jface.resource.ImageDescriptor;

import io.snyk.eclipse.plugin.Activator;

public class SnykIcons {
	public static final ImageDescriptor CODE = Activator.getImageDescriptor("/icons/code.png");
	public static final ImageDescriptor CODE_DISABLED = Activator.getImageDescriptor("/icons/code_disabled.png");

	public static final ImageDescriptor OSS = Activator.getImageDescriptor("/icons/oss.png");
	public static final ImageDescriptor OSS_DISABLED = Activator.getImageDescriptor("/icons/oss_disabled.png");

	public static final ImageDescriptor IAC = Activator.getImageDescriptor("/icons/iac.png");
	public static final ImageDescriptor IAC_DISABLED = Activator.getImageDescriptor("/icons/iac_disabled.png");

	public static final ImageDescriptor SEVERITY_CRITICAL = Activator
			.getImageDescriptor("/icons/severity-critical.png");
	public static final ImageDescriptor SEVERITY_HIGH = Activator.getImageDescriptor("/icons/severity-high.png");
	public static final ImageDescriptor SEVERITY_MEDIUM = Activator.getImageDescriptor("/icons/severity-medium.png");
	public static final ImageDescriptor SEVERITY_LOW = Activator.getImageDescriptor("/icons/severity-low.png");

	public static final ImageDescriptor ENABLED = Activator.getImageDescriptor("/icons/enabled.png");
	public static final ImageDescriptor DISABLED = Activator.getImageDescriptor("/icons/transparent.png");

	public static final ImageDescriptor PROJECT = Activator
			.getImageDescriptor("platform:/plugin/org.eclipse.ui.ide/icons/full/obj16/prj_obj.png");
	public static final ImageDescriptor FILE = Activator
			.getImageDescriptor("platform:/plugin/org.eclipse.ui.genericeditor/icons/full/obj16/generic_editor.png");

}
