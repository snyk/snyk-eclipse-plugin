package io.snyk.eclipse.plugin.views.snyktoolview.filters;

import java.util.function.Predicate;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class SeverityFilter extends BaseFilter {
	private static final Predicate<Issue> predicate = issue -> issue.severity()
			.equals(ProductConstants.SEVERITY_CRITICAL);

	public SeverityFilter(String preferenceKey) {
		super(preferenceKey, predicate);
	}
}