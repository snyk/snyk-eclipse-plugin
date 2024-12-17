package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class EnableOssProductHandler extends BaseHandler implements IElementUpdater {

	public EnableOssProductHandler() {
		super();
		preferenceKey = Preferences.ACTIVATE_SNYK_OPEN_SOURCE;
	}

}