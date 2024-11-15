package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterDeltaOpenIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterDeltaOpenIssuesHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		preferenceKey = Preferences.FILTER_DELTA_OPEN_ISSUES;
	}

}
