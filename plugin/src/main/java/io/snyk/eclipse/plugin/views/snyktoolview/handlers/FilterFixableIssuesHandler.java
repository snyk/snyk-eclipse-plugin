package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.FixableFilter;

public class FilterFixableIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterFixableIssuesHandler() {
		super();
		preferenceKey = Preferences.FILTER_SHOW_ONLY_FIXABLE;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		new FixableFilter(TreeFilterManager.getInstance()).applyFilter();

		return null;
	}

}
