package io.snyk.eclipse.plugin.preferences;

import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IExecutableExtensionFactory;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

/**
 * Factory that creates either the HTML-based or native preference page
 * based on the USE_LS_HTML_CONFIG_DIALOG preference.
 * 
 * The preference can be set via:
 * - Environment variable: SNYK_USE_HTML_SETTINGS=true
 * - System property: snyk.useHtmlSettings=true
 * - Eclipse preference (once set, persists)
 */
public class SnykPreferencePageFactory implements IExecutableExtensionFactory, IExecutableExtension {

	private IConfigurationElement config;
	private String propertyName;

	@Override
	public void setInitializationData(IConfigurationElement config, String propertyName, Object data)
			throws CoreException {
		this.config = config;
		this.propertyName = propertyName;
	}

	@Override
	public Object create() throws CoreException {
		if (Preferences.isNewConfigDialogEnabled()) {
			return new HTMLSettingsPreferencePage();
		} else {
			return new PreferencesPage();
		}
	}
}
