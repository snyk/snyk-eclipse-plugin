package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class EnableIacProductHandler extends BaseHandler implements IElementUpdater {

	public EnableIacProductHandler() {
		super();

		iconEnabled = SnykIcons.IAC;
		iconDisabled = SnykIcons.IAC_DISABLED;
		preferenceKey = Preferences.ACTIVATE_SNYK_IAC;
	}

}