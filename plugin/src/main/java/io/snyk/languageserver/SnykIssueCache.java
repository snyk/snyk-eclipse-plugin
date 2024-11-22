package io.snyk.languageserver;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.IssueComparator;

public class SnykIssueCache {
	private final Map<String, Collection<Issue>> codeSecurityIssues = new ConcurrentHashMap<>();
	private final Map<String, Collection<Issue>> codeQualityIssues = new ConcurrentHashMap<>();
	private final Map<String, Collection<Issue>> ossIssues = new ConcurrentHashMap<>();
	private final Map<String, Collection<Issue>> iacIssues = new ConcurrentHashMap<>();

	private Predicate<? super Issue> fixablePredicate = new Predicate<Issue>() {
		@Override
		public boolean test(Issue issue) {
			return issue != null && (issue.additionalData().hasAIFix() || issue.additionalData().isUpgradable());
		}
	};

	private Predicate<? super Issue> ignoredPredicate = new Predicate<Issue>() {
		@Override
		public boolean test(Issue issue) {
			return issue != null && issue.isIgnored();
		}
	};

	/**
	 * This is public for testing purposes
	 */
	public SnykIssueCache() {
	}

	/** Clears all issue caches */
	public void clearAll() {
		codeSecurityIssues.clear();
		codeQualityIssues.clear();
		ossIssues.clear();
		iacIssues.clear();
	}

	/**
	 * Removes all issues for a given path from all caches
	 *
	 * @param path The file path for which issues should be removed
	 */
	public void removeAllIssuesForPath(String path) {
		codeSecurityIssues.clear();
		codeQualityIssues.clear();
		ossIssues.remove(path);
		iacIssues.remove(path);
	}

	private Collection<Issue> getIssuesForPath(String path, Map<String, Collection<Issue>> cache) {
		Collection<Issue> issues = cache.get(path);
		return issues != null ? Collections.unmodifiableCollection(issues) : Collections.emptyList();
	}

	public Collection<Issue> getIssues(String path, String displayProduct) {
		var cache = getCacheByDisplayProduct(displayProduct);
		return getIssuesForPath(path, cache);
	}

	// ----- Methods for Snyk Code Issues -----

	/**
	 * Adds or updates Snyk Code Security & Quality issues for a given path
	 *
	 * @param path   The file path
	 * @param issues The collection of issues to add
	 */
	public void addCodeIssues(String path, Collection<Issue> issues) {
		var qualityIssues = new TreeSet<Issue>(new IssueComparator(issues));
		var securityIssues = new TreeSet<Issue>(new IssueComparator(issues));
		for (Issue issue : issues) {
			if (issue.additionalData().isSecurityType()) {
				securityIssues.add(issue);
			} else {
				qualityIssues.add(issue);
			}
		}
		if (qualityIssues.size() > 0) {
			codeQualityIssues.put(path, qualityIssues);
		} else {
			codeQualityIssues.remove(path);
		}

		if (securityIssues.size() > 0) {
			codeSecurityIssues.put(path, securityIssues);
		} else {
			codeSecurityIssues.remove(path);
		}
	}

	/**
	 * Retrieves Snyk Code Security issues for a given path
	 *
	 * @param path The file path
	 * @return An unmodifiable collection of issues, or an empty list if none exist
	 */
	public Collection<Issue> getCodeSecurityIssuesForPath(String path) {
		return getIssuesForPath(path, codeSecurityIssues);
	}

	/**
	 * Retrieves Snyk Code Quality issues for a given path
	 *
	 * @param path The file path
	 * @return An unmodifiable collection of issues, or an empty list if none exist
	 */
	public Collection<Issue> getCodeQualityIssuesForPath(String path) {
		return getIssuesForPath(path, codeQualityIssues);
	}

	/**
	 * Removes Snyk Code issues for a given path. Removes both Quality & Security
	 * issues.
	 *
	 * @param path The file path
	 */
	public void removeCodeIssuesForPath(String path) {
		codeSecurityIssues.remove(path);
		codeQualityIssues.remove(path);
	}

	// ----- Methods for Snyk Open Source Issues -----

	/**
	 * Adds or updates Snyk Open Source issues for a given path
	 *
	 * @param path   The file path
	 * @param issues The collection of issues to add
	 */
	public void addOssIssues(String path, Collection<Issue> issues) {
		if (issues.size() > 0) {
			ossIssues.put(path, new TreeSet<Issue>(new IssueComparator(issues)));
		} else {
			ossIssues.remove(path);
		}
	}

	/**
	 * Retrieves Snyk Open Source issues for a given path
	 *
	 * @param path The file path
	 * @return An unmodifiable collection of issues, or an empty list if none exist
	 */
	public Collection<Issue> getOssIssuesForPath(String path) {
		return getIssuesForPath(path, ossIssues);
	}

	/**
	 * Removes Snyk Open Source issues for a given path
	 *
	 * @param path The file path
	 */
	public void removeOssIssuesForPath(String path) {
		ossIssues.remove(path);
	}

	// ----- Methods for Snyk IaC Issues -----

	/**
	 * Adds or updates Snyk IaC issues for a given path
	 *
	 * @param path   The file path
	 * @param issues The collection of issues to add
	 */
	public void addIacIssues(String path, Collection<Issue> issues) {
		if (issues.size() > 0) {
			iacIssues.put(path, new TreeSet<Issue>(new IssueComparator(issues)));
		} else {
			iacIssues.remove(path);
		}
	}

	/**
	 * Retrieves Snyk IaC issues for a given path
	 *
	 * @param path The file path
	 * @return An unmodifiable collection of issues, or an empty list if none exist
	 */
	public Collection<Issue> getIacIssuesForPath(String path) {
		return getIssuesForPath(path, iacIssues);
	}

	/**
	 * Removes Snyk IaC issues for a given path
	 *
	 * @param path The file path
	 */
	public void removeIacIssuesForPath(String path) {
		iacIssues.remove(path);
	}

	/**
	 * Return the total count
	 *
	 * @param product displayed product as defined in ProductConstants
	 * @return
	 */
	public long getTotalCount(String product) {
		return getCacheByDisplayProduct(product).values().stream().flatMap(Collection::stream).count();
	}

	public Map<String, Collection<Issue>> getCacheByDisplayProduct(String displayProduct) {
		switch (displayProduct) {
		case ProductConstants.DISPLAYED_OSS:
			return ossIssues;
		case ProductConstants.DISPLAYED_IAC:
			return iacIssues;
		case ProductConstants.DISPLAYED_CODE_SECURITY:
			return codeSecurityIssues;
		case ProductConstants.DISPLAYED_CODE_QUALITY:
			return codeQualityIssues;
		default:
			throw new IllegalArgumentException("Unexpected value: " + displayProduct);
		}
	}

	public Collection<Issue> getFilteredIssue(String displayProduct, Predicate<? super Issue> filter) {
		var cache = getCacheByDisplayProduct(displayProduct);
		return cache.values().stream().flatMap(Collection::stream).filter(filter).toList();
	}

	public long getFixableCount(String displayProduct) {
		var issues = getFilteredIssue(displayProduct, fixablePredicate);
		return issues.size();
	}

	public long getIgnoredCount(String displayProduct) {
		var issues = getFilteredIssue(displayProduct, ignoredPredicate);
		return issues.size();
	}

	public long getIssueCountBySeverity(String displayProduct, String severity) {
		var issues = getFilteredIssue(displayProduct, new Predicate<Issue>() {
			@Override
			public boolean test(Issue issue) {
				return issue != null && issue.severity().equals(severity);
			}
		});
		return issues.size();
	}
}
