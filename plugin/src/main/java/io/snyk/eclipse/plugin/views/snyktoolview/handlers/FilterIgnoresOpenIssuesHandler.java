package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterIgnoresOpenIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterIgnoresOpenIssuesHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		preferenceKey = Preferences.FILTER_IGNORES_OPEN_ISSUES;
	}

}