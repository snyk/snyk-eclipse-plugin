package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

import java.util.*;
import io.snyk.eclipse.plugin.domain.ProductConstants;

public class IssueComparator implements Comparator<Issue> {

    private final Map<Issue, List<Issue>> issuesGrouped;

    public IssueComparator() {
        this.issuesGrouped = new HashMap<>();
    }

    public IssueComparator(Collection<Issue> issues) {
        this.issuesGrouped = new HashMap<>();
        for (Issue issue : issues) {
            this.issuesGrouped.computeIfAbsent(issue, k -> new ArrayList<>()).add(issue);
        }
    }

    @Override
    public int compare(Issue o1, Issue o2) {
        Map<String, Integer> severityOrder = getSeverityOrderHashMap();

        // Get ranks for the severities of the two issues
        int rank1 = severityOrder.getOrDefault(o1.severity().toLowerCase(), Integer.MAX_VALUE);
        int rank2 = severityOrder.getOrDefault(o2.severity().toLowerCase(), Integer.MAX_VALUE);

        // Compare based on severity rank (lower rank = higher severity)
        int severityComparison = Integer.compare(rank1, rank2);
        if (severityComparison != 0) {
            return severityComparison;
        }

        // Fallback: Compare by issue counts grouped by severity (cascading)
        int o1Criticals = getCount(o1, ProductConstants.SEVERITY_CRITICAL);
        int o2Criticals = getCount(o2, ProductConstants.SEVERITY_CRITICAL);

        int o1Highs = getCount(o1, ProductConstants.SEVERITY_HIGH);
        int o2Highs = getCount(o2, ProductConstants.SEVERITY_HIGH);

        int o1Mediums = getCount(o1, ProductConstants.SEVERITY_MEDIUM);
        int o2Mediums = getCount(o2, ProductConstants.SEVERITY_MEDIUM);

        int o1Lows = getCount(o1, ProductConstants.SEVERITY_LOW);
        int o2Lows = getCount(o2, ProductConstants.SEVERITY_LOW);

        if (o1Criticals != o2Criticals) {
            return Integer.compare(o2Criticals, o1Criticals);
        } else if (o1Highs != o2Highs) {
            return Integer.compare(o2Highs, o1Highs);
        } else if (o1Mediums != o2Mediums) {
            return Integer.compare(o2Mediums, o1Mediums);
        } else if (o1Lows != o2Lows) {
            return Integer.compare(o2Lows, o1Lows);
        }

        // Fallback to comparing by hash codes if everything else is equal
        return Integer.compare(o1.hashCode(), o2.hashCode());
    }

    private int getCount(Issue issue, String severity) {
        List<Issue> issuesForType = issuesGrouped.getOrDefault(issue, Collections.emptyList());
        return (int) issuesForType.stream()
                .filter(i -> severity.equalsIgnoreCase(i.severity()))
                .count();
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
