package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.ToggleSeverityFilters;

public class EnableAllSeveritiesHandler extends BaseHandler implements IElementUpdater {

	public EnableAllSeveritiesHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		iconDisabled = SnykIcons.DISABLED;
		preferenceKey = null;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		new ToggleSeverityFilters(TreeFilterManager.getInstance(), Preferences.getInstance())
				.applyFilter();
		
		//TODO Handle UI updates, tickbox on buttons on view menu

		return null;
	}

}