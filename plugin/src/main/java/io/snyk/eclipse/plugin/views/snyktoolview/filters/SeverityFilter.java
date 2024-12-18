package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.Map;
import java.util.function.Predicate;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class SeverityFilter extends BaseFilter {
	private static Map<String, String> preferenceToProductConstants = Map.of(Preferences.FILTER_SHOW_CRITICAL,
			ProductConstants.SEVERITY_CRITICAL, Preferences.FILTER_SHOW_HIGH, ProductConstants.SEVERITY_HIGH,
			Preferences.FILTER_SHOW_MEDIUM, ProductConstants.SEVERITY_MEDIUM, Preferences.FILTER_SHOW_LOW,
			ProductConstants.SEVERITY_LOW);

	public SeverityFilter(TreeFilterManager tfm, String preferenceKey) {
		super(preferenceKey, getPredicate(preferenceKey), tfm);
	}

	@SuppressWarnings("rawtypes")
	private static Predicate getPredicate(String preferenceKey) {
		String severityCondition = preferenceToProductConstants.get(preferenceKey);
		Predicate<Issue> predicate = issue -> !issue.severity().equals(severityCondition);
		return predicate;
	}	
}