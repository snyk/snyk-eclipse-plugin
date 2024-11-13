package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class FilterCriticalHandler extends BaseHandler implements IElementUpdater {
	
	public FilterCriticalHandler() {
		super();
		// TODO should we replace the filter button with a filter applied button icon?
		iconEnabled = Activator.getImageDescriptor("/icons/severity-critical.png");
		iconDisabled = Activator.getImageDescriptor("/icons/severity-critical.png");
		preferenceKey = Preferences.FILTER_CRITICAL;
	}

}