package io.snyk.eclipse.plugin;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {
	private static final String ICONS_TRANSPARENT_PNG = "/icons/transparent.png";
	private static final String ICONS_ENABLED_PNG = "/icons/enabled.png";
	private static final String ICONS_SEVERITY_LOW_PNG = "/icons/severity_low.png";
	private static final String ICONS_SEVERITY_MEDIUM_PNG = "/icons/severity_medium.png";
	private static final String ICONS_SEVERITY_HIGH_PNG = "/icons/severity_high.png";
	private static final String ICONS_SEVERITY_CRITICAL_PNG = "/icons/severity_critical.png";
	private static final String ICONS_IAC_DISABLED_PNG = "/icons/iac_disabled.png";
	private static final String ICONS_IAC_PNG = "/icons/iac.png";
	private static final String ICONS_OSS_DISABLED_PNG = "/icons/oss_disabled.png";
	private static final String ICONS_OSS_PNG = "/icons/oss.png";
	private static final String ICONS_CODE_DISABLED_PNG = "/icons/code_disabled.png";
	private static final String ICONS_CODE_PNG = "/icons/code.png";
	// The plug-in ID
	public static final String PLUGIN_ID = "io.snyk.eclipse.plugin"; //$NON-NLS-1$
	public static final String PLUGIN_VERSION = Platform.getBundle(Activator.PLUGIN_ID).getVersion().toString();
	public static final String INTEGRATION_NAME = "ECLIPSE";

	// The shared instance
	private static Activator plugin;

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in relative
	 * path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public ImageDescriptor getImageDescriptor(String key) {
		if (Preferences.getInstance().isTest()) return null;
		// Use syncExec to invoke code on the SWT thread and wait for it to finish
		return Display.getDefault().syncCall(() -> {
			var imageDescriptor = getImageRegistry().getDescriptor(key);
			if (imageDescriptor == null) {
				imageDescriptor = ImageDescriptor.getMissingImageDescriptor();
			}
			return imageDescriptor;
		});
	}

	@Override
	protected void initializeImageRegistry(ImageRegistry registry) {
		addImageDescriptorToRegistry(registry, SnykIcons.CODE_ID, ICONS_CODE_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.CODE_DISABLED_ID, ICONS_CODE_DISABLED_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.OSS_ID, ICONS_OSS_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.OSS_DISABLED_ID, ICONS_OSS_DISABLED_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.IAC_ID, ICONS_IAC_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.IAC_DISABLED_ID, ICONS_IAC_DISABLED_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.SEVERITY_CRITICAL_ID, ICONS_SEVERITY_CRITICAL_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.SEVERITY_HIGH_ID, ICONS_SEVERITY_HIGH_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.SEVERITY_MEDIUM_ID, ICONS_SEVERITY_MEDIUM_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.SEVERITY_LOW_ID, ICONS_SEVERITY_LOW_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.ENABLED_ID, ICONS_ENABLED_PNG);
		addImageDescriptorToRegistry(registry, SnykIcons.DISABLED_ID, ICONS_TRANSPARENT_PNG);
	}

	private void addImageDescriptorToRegistry(ImageRegistry registry, String key, String path) {
		ImageDescriptor descriptor = imageDescriptorFromPlugin(PLUGIN_ID, path);
		registry.put(key, descriptor);
	}
}
