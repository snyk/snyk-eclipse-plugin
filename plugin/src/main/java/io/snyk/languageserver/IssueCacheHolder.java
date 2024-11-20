package io.snyk.languageserver;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.core.resources.IProject;

import io.snyk.eclipse.plugin.utils.ResourceUtils;

public class IssueCacheHolder {
	private Map<Path, SnykIssueCache> caches = new ConcurrentHashMap<>();
	private static IssueCacheHolder instance = new IssueCacheHolder();

	public static IssueCacheHolder getInstance() {
		if (instance == null) {
			synchronized (IssueCacheHolder.class) {
				if (instance == null) {
					instance = new IssueCacheHolder();
				}
			}
		}
		return instance;
	}

	public SnykIssueCache getCacheInstance(Path path) {
		for (Path p : caches.keySet()) {
			if (path.startsWith(p)) {
				return caches.get(p);
			}
		}
		caches.put(path, new SnykIssueCache());
		return caches.get(path);
	}

	public SnykIssueCache getCacheInstance(IProject project) {
		return getCacheInstance(ResourceUtils.getFullPath(project));
	}

	public SnykIssueCache getCacheInstance(String folderPath) {
		return getCacheInstance(Paths.get(folderPath));
	}
}
