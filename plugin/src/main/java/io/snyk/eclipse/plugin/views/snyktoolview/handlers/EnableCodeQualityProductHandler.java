package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class EnableCodeQualityProductHandler extends BaseHandler implements IElementUpdater {

	public EnableCodeQualityProductHandler() {
		super();
		// TODO should we replace the filter button with a filter applied button icon?
		iconEnabled = Activator.getImageDescriptor("/icons/code.png");
		iconDisabled = Activator.getImageDescriptor("/icons/code_disabled.png");
		preferenceKey = Preferences.ACTIVATE_SNYK_CODE_QUALITY;
	}

}