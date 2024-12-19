package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_OPEN_SOURCE;

import org.eclipse.ui.commands.IElementUpdater;

public class EnableOssProductHandler extends BaseProductFilterHandler implements IElementUpdater {

	public EnableOssProductHandler() {
		super(ACTIVATE_SNYK_OPEN_SOURCE);
		preferenceKey = ACTIVATE_SNYK_OPEN_SOURCE;
	}

}