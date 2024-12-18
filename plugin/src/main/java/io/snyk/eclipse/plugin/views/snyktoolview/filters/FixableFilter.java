package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_ONLY_FIXABLE;

import java.util.function.Predicate;

import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class FixableFilter extends BaseFilter {	
	private static final Predicate<Issue> predicate = issue -> issue.hasFix();
	
	public FixableFilter(TreeFilterManager tfm) {
		super(FILTER_SHOW_ONLY_FIXABLE, predicate, tfm);
	}

	@Override
	public void applyFilter() {
		// this is inverse to the other filters, so we need to overwrite the BaseFilter logic
		boolean booleanPref = preferences.getBooleanPref(this.filterName);

		if (booleanPref) {
			this.filterManager.addTreeFilter(this.filterName, predicate);
		} else {
			this.filterManager.removeTreeFilter(this.filterName);
		}
	}
	
	
}