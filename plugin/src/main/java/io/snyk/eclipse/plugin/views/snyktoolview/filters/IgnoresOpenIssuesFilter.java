package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES;

import java.util.function.Predicate;

import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class IgnoresOpenIssuesFilter extends BaseFilter {
	private static final Predicate<Issue> predicate = issue -> !issue.isIgnored();
	
	public IgnoresOpenIssuesFilter(TreeFilterManager tfm) {
		super(FILTER_IGNORES_SHOW_OPEN_ISSUES, predicate, tfm);
	}
}
