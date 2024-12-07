package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.Arrays;
import java.util.List;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;

public class ToggleSeverityFilters implements BaseFilter {
	private TreeFilterManager filterManager;
	private Preferences preferences;
	private List<String> severityPreferenceStrings = Arrays.asList(Preferences.FILTER_CRITICAL, Preferences.FILTER_HIGH,
			Preferences.FILTER_MEDIUM, Preferences.FILTER_LOW);

	public ToggleSeverityFilters(TreeFilterManager filterManager, Preferences preferences) {
		this.filterManager = filterManager;
		this.preferences = preferences;
	}

	@Override
	public void applyFilter() {
		// All filters are false by default, so all issues are displayed. But if one of
		// the filters are true, then all filters are removed and all issues are show.
		boolean anyPreferenceFalse = preferences.anyPreferenceTrue(severityPreferenceStrings);

		if (anyPreferenceFalse) {
			this.filterManager.removeFilters(severityPreferenceStrings);
		} else {
			this.filterManager.addFilters(severityPreferenceStrings);
		}
	}
}