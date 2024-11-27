package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

import io.snyk.eclipse.plugin.domain.ProductConstants;

public class IssueComparator implements Comparator<Issue> {
    @Override
    public int compare(Issue o1, Issue o2) {
        Map<String, Integer> severityOrder = getSeverityOrderHashMap();

        int rank1 = severityOrder.getOrDefault(o1.severity().toLowerCase(), Integer.MAX_VALUE);
        int rank2 = severityOrder.getOrDefault(o2.severity().toLowerCase(), Integer.MAX_VALUE);

        // Compare based on severity rank
        int severityComparison = Integer.compare(rank1, rank2);
        if (severityComparison != 0) {
            return severityComparison;
        }

        // Fallback to comparing by hash codes if everything else is equal
        return Integer.compare(o1.hashCode(), o2.hashCode());
    }

    private static Map<String, Integer> getSeverityOrderHashMap() {
        Map<String, Integer> severityOrder = new HashMap<>();
        severityOrder.put(ProductConstants.SEVERITY_CRITICAL.toLowerCase(), 1);
        severityOrder.put(ProductConstants.SEVERITY_HIGH.toLowerCase(), 2);
        severityOrder.put(ProductConstants.SEVERITY_MEDIUM.toLowerCase(), 3);
        severityOrder.put(ProductConstants.SEVERITY_LOW.toLowerCase(), 4);
        return severityOrder;
    }
}
