package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.SeverityMediumFilter;

public class FilterMediumHandler extends BaseHandler implements IElementUpdater {

	public FilterMediumHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		iconDisabled = SnykIcons.DISABLED;
		preferenceKey = Preferences.FILTER_MEDIUM;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		new SeverityMediumFilter(TreeFilterManager.getInstance(), Preferences.getInstance(), preferenceKey)
				.applyFilter();

		return null;
	}
}

