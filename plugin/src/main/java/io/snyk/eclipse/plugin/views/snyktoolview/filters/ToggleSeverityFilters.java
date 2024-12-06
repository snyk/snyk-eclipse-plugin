package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.Arrays;
import java.util.List;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;

public class ToggleSeverityFilters implements BaseFilter {
	private TreeFilterManager filterManager;
	private Preferences preferences;

	public ToggleSeverityFilters(TreeFilterManager filterManager, Preferences preferences) {
		this.filterManager = filterManager;
		this.preferences = preferences;
	}

	@Override
	public void applyFilter() {

		List<String> severityPreferenceStrings = Arrays.asList(Preferences.FILTER_CRITICAL, Preferences.FILTER_HIGH,
				Preferences.FILTER_MEDIUM, Preferences.FILTER_LOW);

		if (anyPreferencesTrue(severityPreferenceStrings)) {
			this.filterManager.removeFilters(severityPreferenceStrings);
		} else {
			this.filterManager.addFilters(severityPreferenceStrings);
		}
	}

	public boolean anyPreferencesTrue(List<String> preferenceKeys) {
		for (String key : preferenceKeys) {
			boolean booleanPref = this.preferences.getBooleanPref(key);
			if (booleanPref) {
				return true;
			}
		}
		return false;
	}
}