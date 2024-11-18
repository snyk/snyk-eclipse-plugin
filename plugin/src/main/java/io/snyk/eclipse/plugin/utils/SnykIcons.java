package io.snyk.eclipse.plugin.utils;

import org.eclipse.jface.resource.ImageDescriptor;

public class SnykIcons {
    private static ImageDescriptorProvider imageProvider;

    static {
        imageProvider = new ActivatorImageDescriptorProvider();
    }

    public static final ImageDescriptor CODE = imageProvider.getImageDescriptor("/icons/code.png");
    public static final ImageDescriptor CODE_DISABLED = imageProvider.getImageDescriptor("/icons/code_disabled.png");

    public static final ImageDescriptor OSS = imageProvider.getImageDescriptor("/icons/oss.png");
    public static final ImageDescriptor OSS_DISABLED = imageProvider.getImageDescriptor("/icons/oss_disabled.png");

    public static final ImageDescriptor IAC = imageProvider.getImageDescriptor("/icons/iac.png");
    public static final ImageDescriptor IAC_DISABLED = imageProvider.getImageDescriptor("/icons/iac_disabled.png");

    public static final ImageDescriptor SEVERITY_CRITICAL = imageProvider.getImageDescriptor("/icons/severity-critical.png");
    public static final ImageDescriptor SEVERITY_HIGH = imageProvider.getImageDescriptor("/icons/severity-high.png");
    public static final ImageDescriptor SEVERITY_MEDIUM = imageProvider.getImageDescriptor("/icons/severity-medium.png");
    public static final ImageDescriptor SEVERITY_LOW = imageProvider.getImageDescriptor("/icons/severity-low.png");

    public static final ImageDescriptor ENABLED = imageProvider.getImageDescriptor("/icons/enabled.png");
    public static final ImageDescriptor DISABLED = imageProvider.getImageDescriptor("/icons/transparent.png");

    // You can add a method to set a custom ImageDescriptorProvider if needed
    public static void setImageDescriptorProvider(ImageDescriptorProvider provider) {
        imageProvider = provider;
    }
}