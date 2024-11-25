package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;

public class SeverityCriticalFilter implements BaseFilter {
	private TreeFilterManager filterManager;
	private Preferences preferences;
	private String preferenceKey;

	public SeverityCriticalFilter(TreeFilterManager filterManager, Preferences preferences, String preferenceKey) {
		this.filterManager = filterManager;
		this.preferences = preferences;
		this.preferenceKey = preferenceKey;
	}

	@Override
	public void applyFilter() {
		boolean booleanPref = this.preferences.getBooleanPref(this.preferenceKey);

		if (booleanPref) {
			this.filterManager.removeTreeFilter(this.preferenceKey);
		} else {
			this.filterManager.addTreeFilter(this.preferenceKey,
					issue -> !issue.severity().equals(ProductConstants.SEVERITY_CRITICAL));
		}

	}
}