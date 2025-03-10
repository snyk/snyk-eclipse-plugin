package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.Map;
import java.util.function.Predicate;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class ProductFilter extends BaseFilter {
	private static Map<String, String> preferenceToProductConstants = Map.of(
			Preferences.ACTIVATE_SNYK_OPEN_SOURCE, ProductConstants.FILTERABLE_ISSUE_OPEN_SOURCE, 
			Preferences.ACTIVATE_SNYK_CODE_QUALITY, ProductConstants.FILTERABLE_ISSUE_CODE_QUALITY,
			Preferences.ACTIVATE_SNYK_CODE_SECURITY, ProductConstants.FILTERABLE_ISSUE_CODE_SECURITY, 
			Preferences.ACTIVATE_SNYK_IAC, ProductConstants.FILTERABLE_ISSUE_INFRASTRUCTURE_AS_CODE);

	public ProductFilter(TreeFilterManager tfm, String filterName) {
		super(filterName, getPredicate(filterName), tfm);
	}

	@SuppressWarnings("rawtypes")
	private static Predicate getPredicate(String filterName) {
		String filterableIssueType = preferenceToProductConstants.get(filterName);
		Predicate<Issue> predicate = issue -> issue.filterableIssueType().equals(filterableIssueType);
		return predicate;
	}
}
