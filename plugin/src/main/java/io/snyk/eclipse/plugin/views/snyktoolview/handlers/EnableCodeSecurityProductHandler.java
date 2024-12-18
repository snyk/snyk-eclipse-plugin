package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_SECURITY;

import org.eclipse.ui.commands.IElementUpdater;

public class EnableCodeSecurityProductHandler extends BaseProductFilterHandler implements IElementUpdater {

	public EnableCodeSecurityProductHandler() {
		super(ACTIVATE_SNYK_CODE_SECURITY);
		preferenceKey = ACTIVATE_SNYK_CODE_SECURITY;
	}
}