package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class FilterHighHandler extends BaseHandler implements IElementUpdater {
	
	public FilterHighHandler() {
		super();
		// TODO should we replace the filter button with a filter applied button icon?
		iconEnabled = Activator.getImageDescriptor("/icons/severity-high.png");
		iconDisabled = Activator.getImageDescriptor("/icons/severity-high.png");
		preferenceKey = Preferences.FILTER_HIGH;
	}

}