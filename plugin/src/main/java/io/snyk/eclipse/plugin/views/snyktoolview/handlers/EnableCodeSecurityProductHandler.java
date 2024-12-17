package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class EnableCodeSecurityProductHandler extends BaseHandler implements IElementUpdater {

	public EnableCodeSecurityProductHandler() {
		super();
		preferenceKey = Preferences.ACTIVATE_SNYK_CODE_SECURITY;
	}

}