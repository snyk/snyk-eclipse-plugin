package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class FilterHighHandler extends BaseSeverityFilterHandler implements IElementUpdater {

	public FilterHighHandler() {
		super();
		preferenceKey = Preferences.FILTER_SHOW_HIGH;
	}
}
