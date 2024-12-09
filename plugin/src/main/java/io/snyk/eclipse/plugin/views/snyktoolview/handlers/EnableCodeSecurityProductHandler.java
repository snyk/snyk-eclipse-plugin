package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class EnableCodeSecurityProductHandler extends BaseHandler implements IElementUpdater {

	public EnableCodeSecurityProductHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		iconDisabled = SnykIcons.DISABLED;
		preferenceKey = Preferences.ACTIVATE_SNYK_CODE_SECURITY;
	}

}