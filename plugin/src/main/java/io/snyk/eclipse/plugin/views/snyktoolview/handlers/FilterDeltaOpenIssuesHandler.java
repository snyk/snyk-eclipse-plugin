package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class FilterDeltaOpenIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterDeltaOpenIssuesHandler() {
		super();

		iconEnabled = Activator.getImageDescriptor("/icons/enabled.png");
		// iconDisabled = Activator.getImageDescriptor("/icons/severity-medium.png");
		preferenceKey = Preferences.FILTER_DELTA_OPEN_ISSUES;
	}

}