package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class EnableCodeQualityProductHandler extends BaseHandler implements IElementUpdater {

	public EnableCodeQualityProductHandler() {
		super();

		iconEnabled = SnykIcons.CODE;
		iconDisabled = SnykIcons.CODE_DISABLED;
		preferenceKey = Preferences.ACTIVATE_SNYK_CODE_QUALITY;
	}

}