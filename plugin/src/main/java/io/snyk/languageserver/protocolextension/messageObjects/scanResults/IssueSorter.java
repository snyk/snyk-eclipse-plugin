package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.snyk.eclipse.plugin.domain.ProductConstants;

public class IssueSorter {
    public static List<Issue> sortIssuesBySeverity(Collection<Issue> issues) {
        List<Issue> result = new ArrayList<>(issues);
        Map<String, Integer> severityOrder = getSeverityOrderHashMap();
        result.sort((i1, i2) -> {
            Integer rank1 = severityOrder.getOrDefault(i1.severity().toLowerCase(), Integer.MAX_VALUE);
            Integer rank2 = severityOrder.getOrDefault(i2.severity().toLowerCase(), Integer.MAX_VALUE);
            return rank1.compareTo(rank2);
        });
        return result;
    }

	private static Map<String, Integer> getSeverityOrderHashMap() {
		Map<String, Integer> severityOrder = new HashMap<>();
        severityOrder.put(ProductConstants.SEVERITY_CRITICAL, 1);
        severityOrder.put(ProductConstants.SEVERITY_HIGH, 2);
        severityOrder.put(ProductConstants.SEVERITY_MEDIUM, 3);
        severityOrder.put(ProductConstants.SEVERITY_LOW, 4);
		return severityOrder;
	}
}