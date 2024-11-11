package io.snyk.languageserver;

import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.ConcurrentHashMap;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class SnykIssueCache {
    private static SnykIssueCache instance = new SnykIssueCache();

    private final ConcurrentHashMap<String, Collection<Issue>> snykCodeIssueHashMap = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Collection<Issue>> snykOssIssueHashMap = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Collection<Issue>> snykIaCIssueHashMap = new ConcurrentHashMap<>();

    private SnykIssueCache() {
    }

	public static SnykIssueCache getInstance() {
		if (instance == null) {
			synchronized (SnykIssueCache.class) {
				if (instance == null) {
					instance = new SnykIssueCache();
				}
			}
		}
		return instance;
	}
	
    /** Clears all issue caches */
    public void clearAll() {
        snykCodeIssueHashMap.clear();
        snykOssIssueHashMap.clear();
        snykIaCIssueHashMap.clear();
    }

    /**
     * Removes all issues for a given path from all caches
     *
     * @param path The file path for which issues should be removed
     */
    public void removeAllIssuesForPath(String path) {
        snykCodeIssueHashMap.remove(path);
        snykOssIssueHashMap.remove(path);
        snykIaCIssueHashMap.remove(path);
    }

    // ----- Methods for Snyk Code Issues -----

    /**
     * Adds or updates Snyk Code issues for a given path
     *
     * @param path   The file path
     * @param issues The collection of issues to add
     */
    public void addCodeIssues(String path, Collection<Issue> issues) {
        snykCodeIssueHashMap.put(path, issues);
    }

    /**
     * Retrieves Snyk Code issues for a given path
     *
     * @param path The file path
     * @return An unmodifiable collection of issues, or an empty list if none exist
     */
    public Collection<Issue> getCodeIssuesForPath(String path) {
        Collection<Issue> issues = snykCodeIssueHashMap.get(path);
        return issues != null ? Collections.unmodifiableCollection(issues) : Collections.emptyList();
    }

    /**
     * Removes Snyk Code issues for a given path
     *
     * @param path The file path
     */
    public void removeCodeIssuesForPath(String path) {
        snykCodeIssueHashMap.remove(path);
    }

    // ----- Methods for Snyk Open Source Issues -----

    /**
     * Adds or updates Snyk Open Source issues for a given path
     *
     * @param path   The file path
     * @param issues The collection of issues to add
     */
    public void addOssIssues(String path, Collection<Issue> issues) {
        snykOssIssueHashMap.put(path, issues);
    }

    /**
     * Retrieves Snyk Open Source issues for a given path
     *
     * @param path The file path
     * @return An unmodifiable collection of issues, or an empty list if none exist
     */
    public Collection<Issue> getOssIssuesForPath(String path) {
        Collection<Issue> issues = snykOssIssueHashMap.get(path);
        return issues != null ? Collections.unmodifiableCollection(issues) : Collections.emptyList();
    }

    /**
     * Removes Snyk Open Source issues for a given path
     *
     * @param path The file path
     */
    public void removeOssIssuesForPath(String path) {
        snykOssIssueHashMap.remove(path);
    }

    // ----- Methods for Snyk IaC Issues -----

    /**
     * Adds or updates Snyk IaC issues for a given path
     *
     * @param path   The file path
     * @param issues The collection of issues to add
     */
    public void addIacIssues(String path, Collection<Issue> issues) {
        snykIaCIssueHashMap.put(path, issues);
    }

    /**
     * Retrieves Snyk IaC issues for a given path
     *
     * @param path The file path
     * @return An unmodifiable collection of issues, or an empty list if none exist
     */
    public Collection<Issue> getIacIssuesForPath(String path) {
        Collection<Issue> issues = snykIaCIssueHashMap.get(path);
        return issues != null ? Collections.unmodifiableCollection(issues) : Collections.emptyList();
    }

    /**
     * Removes Snyk IaC issues for a given path
     *
     * @param path The file path
     */
    public void removeIacIssuesForPath(String path) {
        snykIaCIssueHashMap.remove(path);
    }
}
