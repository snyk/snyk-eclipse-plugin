package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.Arrays;
import java.util.List;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;

public class ToggleProductFilters implements BaseFilter {
	private TreeFilterManager filterManager;
	private Preferences preferences;
	private List<String> productPreferenceStrings = Arrays.asList(Preferences.ACTIVATE_SNYK_OPEN_SOURCE,
			Preferences.ACTIVATE_SNYK_CODE_SECURITY, Preferences.ACTIVATE_SNYK_CODE_QUALITY,
			Preferences.ACTIVATE_SNYK_IAC);

	public ToggleProductFilters(TreeFilterManager filterManager, Preferences preferences) {
		this.filterManager = filterManager;
		this.preferences = preferences;
	}

	@Override
	public void applyFilter() {
		// OSS and IAC are enabled by default, when all products are selected then all
		// products are enabled if any one of them are disabled (== false).
		boolean anyPreferenceFalse = preferences.anyPreferenceFalse(productPreferenceStrings);

		if (anyPreferenceFalse) {
			this.filterManager.removeFilters(productPreferenceStrings);
		} else {
			this.filterManager.addFilters(productPreferenceStrings);
		}
	}
}