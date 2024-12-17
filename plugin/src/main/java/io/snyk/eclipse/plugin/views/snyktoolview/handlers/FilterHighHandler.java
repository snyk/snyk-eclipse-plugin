package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.SeverityFilter;

public class FilterHighHandler extends BaseSeverityFilterHandler implements IElementUpdater {

	public FilterHighHandler() {
		super();
		preferenceKey = Preferences.FILTER_SHOW_HIGH;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		new SeverityFilter(preferenceKey).applyFilter();

		return null;
	}
}
