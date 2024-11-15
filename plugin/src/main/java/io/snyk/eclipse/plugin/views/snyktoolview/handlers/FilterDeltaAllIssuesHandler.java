package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterDeltaAllIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterDeltaAllIssuesHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		preferenceKey = Preferences.FILTER_DELTA_ALL_ISSUES;
	}

}