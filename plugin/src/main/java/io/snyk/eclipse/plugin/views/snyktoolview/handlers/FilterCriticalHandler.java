package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterCriticalHandler extends BaseHandler implements IElementUpdater {

	public FilterCriticalHandler() {
		super();

		iconEnabled = SnykIcons.SEVERITY_CRITICAL;
		iconDisabled = SnykIcons.SEVERITY_CRITICAL;
		preferenceKey = Preferences.FILTER_CRITICAL;
	}

}