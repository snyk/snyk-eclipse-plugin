package io.snyk.eclipse.plugin.views.snyktoolview.handlers;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_SECURITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_IAC;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_OPEN_SOURCE;

import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.utils.SnykIcons;

public class EnableAllProductHandler extends BaseHandler implements IElementUpdater {
	public EnableAllProductHandler() {
		super();
		iconEnabled = SnykIcons.getImageDescriptor(SnykIcons.ENABLED_ID);
		iconDisabled = SnykIcons.getImageDescriptor(SnykIcons.DISABLED_ID);
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		List<String> productPrefs = List.of(ACTIVATE_SNYK_CODE_SECURITY, ACTIVATE_SNYK_IAC, ACTIVATE_SNYK_OPEN_SOURCE);

		updateMultiplePrefs(productPrefs);

		return null;
	}
}