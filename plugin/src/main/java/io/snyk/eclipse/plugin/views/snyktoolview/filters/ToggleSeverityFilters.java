package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.Arrays;
import java.util.List;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.languageserver.LsConfigurationUpdater;

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
		boolean anyPreferenceTrue = preferences.anyPreferenceTrue(severityPreferenceStrings);

		if (anyPreferenceTrue) {
			this.filterManager.removeFilters(severityPreferenceStrings);

			// Return all severity preferences to default; false.
			this.preferences.setPreferences(severityPreferenceStrings, false);
		} else {
			this.filterManager.addFilters(severityPreferenceStrings);

			// If no severity preferences are in use, this will enable all the filters.
			this.preferences.setPreferences(severityPreferenceStrings, true);
		}

		new LsConfigurationUpdater().configurationChanged();
	}
}