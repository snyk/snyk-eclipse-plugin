package io.snyk.languageserver;

import java.io.File;
import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class SnykIssueCache {
	private SnykIssueCache() {
		
	}
	
	private final ConcurrentHashMap<File, Collection<Issue>> snykCodeIssueHashMap = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<File, Collection<Issue>> snykOssIssueHashMap = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<File, Collection<Issue>> snykIaCIssueHashMap = new ConcurrentHashMap<>();
    
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
    
    public ConcurrentHashMap<File, Collection<Issue>> getSnykCodeIssueHashMap() {
		return snykCodeIssueHashMap;
	}
	public ConcurrentHashMap<File, Collection<Issue>> getSnykOssIssueHashMap() {
		return snykOssIssueHashMap;
	}
	public ConcurrentHashMap<File, Collection<Issue>> getSnykIaCIssueHashMap() {
		return snykIaCIssueHashMap;
	}

}
