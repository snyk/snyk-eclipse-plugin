package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES;

import java.util.function.Predicate;

import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class IgnoresIgnoredIssuesFilter extends BaseFilter {
	private static final Predicate<Issue> predicate = issue -> !issue.isIgnored();

	public IgnoresIgnoredIssuesFilter(TreeFilterManager tfm) {
		super(FILTER_IGNORES_SHOW_IGNORED_ISSUES, predicate, tfm);
	}

}
