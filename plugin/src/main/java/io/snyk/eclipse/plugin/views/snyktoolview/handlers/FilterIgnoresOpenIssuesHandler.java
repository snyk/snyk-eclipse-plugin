package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class FilterIgnoresOpenIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterIgnoresOpenIssuesHandler() {
		super();
		preferenceKey = Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		SnykExtendedLanguageClient.getInstance().updateConfiguration();

		return null;
	}
}
