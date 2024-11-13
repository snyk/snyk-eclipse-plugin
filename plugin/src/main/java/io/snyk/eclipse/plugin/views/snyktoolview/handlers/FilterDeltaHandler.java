package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class FilterDeltaHandler extends BaseHandler implements IElementUpdater {

	public FilterDeltaHandler() {
		super();
		// TODO should we replace the filter button with a filter applied button icon?
		// TODO update to the actual Delta findings icons, if needed.
		iconEnabled = Activator.getImageDescriptor("/icons/code.png");
		iconDisabled = Activator.getImageDescriptor("/icons/code_disabled.png");
		preferenceKey = Preferences.FILTER_DELTA;
	}

}