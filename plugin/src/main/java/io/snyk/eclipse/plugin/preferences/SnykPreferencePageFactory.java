package io.snyk.eclipse.plugin.preferences;

import org.eclipse.core.runtime.IExecutableExtensionFactory;
import org.eclipse.core.runtime.CoreException;

/**
 * Factory that creates either the HTML-based or native preference page
 * based on the USE_LS_HTML_CONFIG_DIALOG preference.
 * 
 * The preference can be set via:
 * - Environment variable: SNYK_USE_HTML_SETTINGS=true
 * - System property: snyk.useHtmlSettings=true
 * - Eclipse preference (once set, persists)
 */
public class SnykPreferencePageFactory implements IExecutableExtensionFactory {

	@Override
	public Object create() throws CoreException {
		if (Preferences.isNewConfigDialogEnabled()) {
			return new HTMLSettingsPreferencePage();
		} else {
			return new PreferencesPage();
		}
	}
}
