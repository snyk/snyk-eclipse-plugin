package io.snyk.languageserver;

import java.io.File;
import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class SnykIssueCache {
	private SnykIssueCache() {
		
	}
	
	private final ConcurrentHashMap<String, Collection<Issue>> snykCodeIssueHashMap = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Collection<Issue>> snykOssIssueHashMap = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, Collection<Issue>> snykIaCIssueHashMap = new ConcurrentHashMap<>();
    
    private static SnykIssueCache instance = new SnykIssueCache();    
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
    
    public ConcurrentHashMap<String, Collection<Issue>> getSnykCodeIssueHashMap() {
		return snykCodeIssueHashMap;
	}
	public ConcurrentHashMap<String, Collection<Issue>> getSnykOssIssueHashMap() {
		return snykOssIssueHashMap;
	}
	public ConcurrentHashMap<String, Collection<Issue>> getSnykIaCIssueHashMap() {
		return snykIaCIssueHashMap;
	}

}
