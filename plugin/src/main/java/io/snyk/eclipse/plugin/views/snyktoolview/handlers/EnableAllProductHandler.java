package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_QUALITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_SECURITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_IAC;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_OPEN_SOURCE;

import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class EnableAllProductHandler extends BaseHandler implements IElementUpdater {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		List<String> productPrefs = List.of(ACTIVATE_SNYK_CODE_QUALITY, ACTIVATE_SNYK_CODE_SECURITY, ACTIVATE_SNYK_IAC,
				ACTIVATE_SNYK_OPEN_SOURCE);

		updateMultiplePrefs(productPrefs);

		SnykExtendedLanguageClient.getInstance().updateConfiguration();
		return null;
	}
}