package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.FixableFilter;

public class FilterFixableIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterFixableIssuesHandler() {
		super();
		preferenceKey = Preferences.FILTER_FIXABLE_ISSUES;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		new FixableFilter(preferenceKey).applyFilter();

		return null;
	}

}
