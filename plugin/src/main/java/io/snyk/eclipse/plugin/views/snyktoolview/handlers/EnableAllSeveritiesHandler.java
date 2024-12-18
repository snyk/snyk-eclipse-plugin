package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_CRITICAL;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_HIGH;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_LOW;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_MEDIUM;

import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class EnableAllSeveritiesHandler extends BaseHandler implements IElementUpdater {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		List<String> preferenceOptions = List.of(FILTER_SHOW_CRITICAL, FILTER_SHOW_HIGH, FILTER_SHOW_MEDIUM,
				FILTER_SHOW_LOW);

		updateMultiplePrefs(preferenceOptions);

		SnykExtendedLanguageClient.getInstance().updateConfiguration();
		return null;
	}

}