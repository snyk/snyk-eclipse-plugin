package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class EnableIacProductHandler extends BaseHandler implements IElementUpdater {

	public EnableIacProductHandler() {
		super();
		preferenceKey = Preferences.ACTIVATE_SNYK_IAC;
	}

}