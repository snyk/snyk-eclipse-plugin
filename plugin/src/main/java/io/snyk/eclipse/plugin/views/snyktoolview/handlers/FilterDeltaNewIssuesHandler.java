package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.preferences.Preferences;

public class FilterDeltaNewIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterDeltaNewIssuesHandler() {
		super();
		preferenceKey = Preferences.ENABLE_DELTA;

	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		boolean booleanPref = Preferences.getInstance().getBooleanPref(this.preferenceKey);

		if (booleanPref) {
			SnykStartup.getView().enableDelta();
		} else {
			SnykStartup.getView().disableDelta();
		}

		return null;
	}

}
