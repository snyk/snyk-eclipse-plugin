package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterDeltaNewIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterDeltaNewIssuesHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		preferenceKey = Preferences.FILTER_DELTA_NEW_ISSUES;

		// TODO filter to only show the issues on local branch
	}

}
