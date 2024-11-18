package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterLowHandler extends BaseHandler implements IElementUpdater {

	public FilterLowHandler() {
		super();

		iconEnabled = SnykIcons.SEVERITY_LOW;
		iconDisabled = SnykIcons.SEVERITY_LOW;
		preferenceKey = Preferences.FILTER_LOW;
	}

}