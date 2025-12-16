package io.snyk.eclipse.plugin.properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IExecutableExtensionFactory;

import io.snyk.eclipse.plugin.preferences.Preferences;

/**
 * Factory that creates either the redirect page (for HTML settings mode) or 
 * the native project property page based on the USE_LS_HTML_CONFIG_DIALOG preference.
 */
public class ProjectPropertyPageFactory implements IExecutableExtensionFactory {

	@Override
	public Object create() throws CoreException {
		if (Preferences.isNewConfigDialogEnabled()) {
			return new ProjectPropertyPage();
		} else {
			return new NativeProjectPropertyPage();
		}
	}
}
