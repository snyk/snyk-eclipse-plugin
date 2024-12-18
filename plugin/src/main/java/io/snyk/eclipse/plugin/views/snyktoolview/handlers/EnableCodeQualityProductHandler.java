package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_QUALITY;

import org.eclipse.ui.commands.IElementUpdater;

public class EnableCodeQualityProductHandler extends BaseProductFilterHandler implements IElementUpdater {

	public EnableCodeQualityProductHandler() {
		super(ACTIVATE_SNYK_CODE_QUALITY);
		preferenceKey = ACTIVATE_SNYK_CODE_QUALITY;
	}
}