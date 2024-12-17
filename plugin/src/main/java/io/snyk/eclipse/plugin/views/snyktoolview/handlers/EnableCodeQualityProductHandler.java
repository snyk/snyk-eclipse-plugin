package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class EnableCodeQualityProductHandler extends BaseHandler implements IElementUpdater {

	public EnableCodeQualityProductHandler() {
		super();
		preferenceKey = Preferences.ACTIVATE_SNYK_CODE_QUALITY;
	}

}