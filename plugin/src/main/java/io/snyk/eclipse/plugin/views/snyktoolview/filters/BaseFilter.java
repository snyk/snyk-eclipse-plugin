package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.function.Predicate;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;

@SuppressWarnings({"rawtypes"})
public abstract class BaseFilter {
	protected Preferences preferences = Preferences.getInstance();
	protected TreeFilterManager filterManager;
	protected String filterName;
	protected Predicate predicate;

	public BaseFilter(String filterName, Predicate predicate, TreeFilterManager tfm) {
		this.filterName = filterName;
		this.predicate = predicate;
		this.filterManager = tfm;
	}

	@SuppressWarnings("unchecked")
	public void applyFilter() {
		boolean booleanPref = this.preferences.getBooleanPref(this.filterName);

		if (booleanPref) {
			this.filterManager.removeTreeFilter(this.filterName); // Showing "a thing" removes it from being filtered out.
		} else {
			this.filterManager.addTreeFilter(this.filterName, predicate); // Hiding (unchecking) "a thing" adds a filter to filter it out.
		}
	}
}
