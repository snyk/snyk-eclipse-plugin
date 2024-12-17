package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.function.Predicate;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class IgnoresFilter extends BaseFilter {
		private static final Predicate<Issue> predicate = issue -> issue.isIgnored();
		
	
		public IgnoresFilter(String preferenceKey) {
			super(preferenceKey, predicate);
		}
}