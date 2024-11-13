package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class EnableOssProductHandler extends BaseHandler implements IElementUpdater {
	
	public EnableOssProductHandler() {
		super();
		// TODO should we replace the filter button with a filter applied button icon?
		iconEnabled = Activator.getImageDescriptor("/icons/oss.png");
		iconDisabled = Activator.getImageDescriptor("/icons/oss_disabled.png");
		preferenceKey = Preferences.ACTIVATE_SNYK_OPEN_SOURCE;
	}

}