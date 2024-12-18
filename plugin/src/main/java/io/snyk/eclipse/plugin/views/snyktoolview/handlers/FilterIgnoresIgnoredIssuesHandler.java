package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.IgnoresIgnoredIssuesFilter;

public class FilterIgnoresIgnoredIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterIgnoresIgnoredIssuesHandler() {
		super();
		preferenceKey = Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		new IgnoresIgnoredIssuesFilter(TreeFilterManager.getInstance()).applyFilter();

		return null;
	}
}
