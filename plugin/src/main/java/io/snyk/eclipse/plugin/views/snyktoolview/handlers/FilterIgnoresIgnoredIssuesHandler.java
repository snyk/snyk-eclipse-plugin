package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterIgnoresIgnoredIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterIgnoresIgnoredIssuesHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		iconDisabled = SnykIcons.DISABLED;
		preferenceKey = Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES;
	}
}