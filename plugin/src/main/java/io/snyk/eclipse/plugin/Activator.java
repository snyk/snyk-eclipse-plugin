package io.snyk.eclipse.plugin;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {
	// The plug-in ID
	public static final String PLUGIN_ID = "io.snyk.eclipse.plugin"; //$NON-NLS-1$
	public static final String PLUGIN_VERSION = Platform.getBundle(Activator.PLUGIN_ID).getVersion().toString();
	public static final String INTEGRATION_NAME = "ECLIPSE";

	// The shared instance
	private static Activator plugin;

	private static ImageRegistry imageRegistry = null; // NOPMD

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
	public static ImageDescriptor getImageDescriptor(String key) {
		Display display = Display.getDefault();

		// Use syncExec to invoke code on the SWT thread and wait for it to finish
		display.syncExec(new Runnable() {
			@Override
			public void run() {
				imageRegistry = getDefault().getImageRegistry();

			}

		});
		if (imageRegistry == null) {
			return ImageDescriptor.getMissingImageDescriptor();

		}
		return imageRegistry.getDescriptor(key);
	}

	@Override
	protected void initializeImageRegistry(ImageRegistry registry) {
		addImageToRegistry(registry, "CODE", "/icons/code.png");
		addImageToRegistry(registry, "CODE_DISABLED", "/icons/code_disabled.png");
		addImageToRegistry(registry, "OSS", "/icons/oss.png");
		addImageToRegistry(registry, "OSS_DISABLED", "/icons/oss_disabled.png");
		addImageToRegistry(registry, "IAC", "/icons/iac.png");
		addImageToRegistry(registry, "IAC_DISABLED", "/icons/iac_disabled.png");
		addImageToRegistry(registry, "SEVERITY_CRITICAL", "/icons/severity_critical.png");
		addImageToRegistry(registry, "SEVERITY_HIGH", "/icons/severity_high.png");
		addImageToRegistry(registry, "SEVERITY_MEDIUM", "/icons/severity_medium.png");
		addImageToRegistry(registry, "SEVERITY_LOW", "/icons/severity_low.png");
		addImageToRegistry(registry, "ENABLED", "/icons/enabled.png");
		addImageToRegistry(registry, "DISABLED", "/icons/transparent.png");
	}

	private void addImageToRegistry(ImageRegistry registry, String key, String path) {
		ImageDescriptor descriptor = imageDescriptorFromPlugin(PLUGIN_ID, path);
		registry.put(key, descriptor);

	}
}
