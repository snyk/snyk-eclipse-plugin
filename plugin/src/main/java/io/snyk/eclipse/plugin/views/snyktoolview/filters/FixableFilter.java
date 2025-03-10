package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_ONLY_FIXABLE;

import java.util.function.Predicate;

import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class FixableFilter extends BaseFilter {	
	private static final Predicate<Issue> predicate = issue -> !issue.hasFix(); // Inverse predicate, see below.
	
	public FixableFilter(TreeFilterManager tfm) {
		super(FILTER_SHOW_ONLY_FIXABLE, predicate, tfm);
	}

	@Override
	public void applyFilter() {
		// This is inverse to the other filters, so we need to overwrite the BaseFilter logic.
		// Deselecting show fixable issues should not hide fixable issues and show all issues.
		// Therefore when deactivated we cannot have a filter added, so the logic is flipped.
		boolean booleanPref = preferences.getBooleanPref(this.filterName);

		if (booleanPref) {
			this.filterManager.addTreeFilter(this.filterName, predicate);
		} else {
			this.filterManager.removeTreeFilter(this.filterName);
		}
	}
	
	
}