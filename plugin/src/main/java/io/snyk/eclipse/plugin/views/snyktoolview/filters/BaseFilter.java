package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.function.Predicate;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;

@SuppressWarnings("rawtypes")
public abstract class BaseFilter {
	private TreeFilterManager filterManager;
	private Preferences preferences;
	protected String preferenceKey;
	protected Predicate predicate;

	public BaseFilter(String preferenceKey, Predicate predicate) {
		this.preferenceKey = preferenceKey;
		this.predicate = predicate;
	}

	@SuppressWarnings("unchecked")
	public void applyFilter() {
		boolean booleanPref = this.preferences.getBooleanPref(this.preferenceKey);

		if (booleanPref) {
			this.filterManager.addTreeFilter(this.preferenceKey, predicate);
		} else {
			this.filterManager.removeTreeFilter(this.preferenceKey);
		}
	}
}