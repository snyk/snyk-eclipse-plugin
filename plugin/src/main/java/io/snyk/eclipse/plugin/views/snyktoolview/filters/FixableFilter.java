package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.function.Predicate;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class FixableFilter extends BaseFilter {	
	private static final Predicate<Issue> predicate = issue -> issue.hasFix();
	
	public FixableFilter(String preferenceKey) {
		super(preferenceKey, predicate);
	}
}