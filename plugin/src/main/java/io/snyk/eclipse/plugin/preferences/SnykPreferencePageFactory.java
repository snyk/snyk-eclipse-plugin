package io.snyk.eclipse.plugin.preferences;

import org.eclipse.core.runtime.IExecutableExtensionFactory;
import org.eclipse.core.runtime.CoreException;

public class SnykPreferencePageFactory implements IExecutableExtensionFactory {

	@Override
	public Object create() throws CoreException {
		return new HTMLSettingsPreferencePage();
	}
}
