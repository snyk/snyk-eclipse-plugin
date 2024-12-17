package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.function.Predicate;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class IgnoresOpenIssuesFilter extends BaseFilter {
	// FIXME think about what happens if this and ignores are both set
	private static final Predicate<Issue> predicate = issue -> !issue.isIgnored();

	public IgnoresOpenIssuesFilter(String preferenceKey) {
		super(preferenceKey, predicate);
	}
}