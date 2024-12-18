package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class FilterMediumHandler extends BaseSeverityFilterHandler implements IElementUpdater {

	public FilterMediumHandler() {
		super();
		preferenceKey = Preferences.FILTER_SHOW_MEDIUM;
	}
}
