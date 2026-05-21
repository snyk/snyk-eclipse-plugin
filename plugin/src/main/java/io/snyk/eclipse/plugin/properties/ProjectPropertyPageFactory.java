package io.snyk.eclipse.plugin.properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IExecutableExtensionFactory;

public class ProjectPropertyPageFactory implements IExecutableExtensionFactory {

	@Override
	public Object create() throws CoreException {
		return new ProjectPropertyPage();
	}
}
