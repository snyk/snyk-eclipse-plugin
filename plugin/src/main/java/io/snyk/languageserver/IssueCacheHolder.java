package io.snyk.languageserver;

import static io.snyk.eclipse.plugin.utils.ResourceUtils.getFullPath;
import static io.snyk.eclipse.plugin.utils.ResourceUtils.getProjectByPath;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.core.resources.IProject;

public class IssueCacheHolder {
	private Map<Path, SnykIssueCache> caches = new ConcurrentHashMap<>();
	private static IssueCacheHolder instance = new IssueCacheHolder();

	public static IssueCacheHolder getInstance() {
		synchronized (IssueCacheHolder.class) {
			if (instance == null) {
				if (instance == null) {
					instance = new IssueCacheHolder();
				}
			}
		}
		return instance;
	}

	public SnykIssueCache getCacheInstance(Path path) {
		for (var entry : caches.entrySet()) {
			var p = entry.getKey();
			if (path.startsWith(p)) {
				return entry.getValue();
			}
		}
		
		var project = getProjectByPath(path);
		if (project == null) {
			return null;
		}
		Path projectPath = getFullPath(project);
		caches.putIfAbsent(projectPath, new SnykIssueCache(projectPath));
		
		return caches.get(projectPath);
	}

	public SnykIssueCache getCacheInstance(IProject project) {
		return getCacheInstance(getFullPath(project));
	}

	public SnykIssueCache getCacheInstance(String folderPath) {
		return getCacheInstance(Paths.get(folderPath));
	}

	public void addCacheForTest(SnykIssueCache cache) {
		caches.put(cache.basePath, cache);
	}
}
