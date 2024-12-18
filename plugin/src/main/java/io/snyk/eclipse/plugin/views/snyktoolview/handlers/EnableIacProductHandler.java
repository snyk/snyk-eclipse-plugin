package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_IAC;

import org.eclipse.ui.commands.IElementUpdater;

public class EnableIacProductHandler extends BaseProductFilterHandler implements IElementUpdater {

	public EnableIacProductHandler() {
		super(ACTIVATE_SNYK_IAC);
		preferenceKey = ACTIVATE_SNYK_IAC;
	}

}