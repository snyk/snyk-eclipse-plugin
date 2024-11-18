package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterMediumHandler extends BaseHandler implements IElementUpdater {

	public FilterMediumHandler() {
		super();

		iconEnabled = SnykIcons.SEVERITY_MEDIUM;
		iconDisabled = SnykIcons.SEVERITY_MEDIUM;
		preferenceKey = Preferences.FILTER_MEDIUM;
	}

}