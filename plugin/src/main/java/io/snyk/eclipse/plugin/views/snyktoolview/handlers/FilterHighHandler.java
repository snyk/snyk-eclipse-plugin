package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterHighHandler extends BaseHandler implements IElementUpdater {
	
	public FilterHighHandler() {
		super();

		iconEnabled = SnykIcons.SEVERITY_HIGH;
		iconDisabled = SnykIcons.SEVERITY_HIGH;
		preferenceKey = Preferences.FILTER_HIGH;
	}

}