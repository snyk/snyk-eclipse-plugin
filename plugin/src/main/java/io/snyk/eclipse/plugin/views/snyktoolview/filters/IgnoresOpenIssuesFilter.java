package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;

public class IgnoresOpenIssuesFilter implements BaseFilter {
	private TreeFilterManager filterManager;
	private Preferences preferences;
	private String preferenceKey;

	public IgnoresOpenIssuesFilter(TreeFilterManager filterManager, Preferences preferences, String preferenceKey) {
		this.filterManager = filterManager;
		this.preferences = preferences;
		this.preferenceKey = preferenceKey;
	}

	@Override
	public void applyFilter() {
		boolean booleanPref = this.preferences.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES);

		if (booleanPref) {
			filterManager.removeTreeFilter(preferenceKey);
		} else {
			filterManager.addTreeFilter(preferenceKey, issue -> issue.isIgnored());
		}

	}
}