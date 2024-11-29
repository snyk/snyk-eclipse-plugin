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
		
		var project = getProjectByPath(path);
		Path projectPath = getFullPath(project);
		caches.put(projectPath, new SnykIssueCache(projectPath));
		
		return caches.get(projectPath);
	}

	public SnykIssueCache getCacheInstance(IProject project) {
		return getCacheInstance(getFullPath(project));
	}

	public SnykIssueCache getCacheInstance(String folderPath) {
		return getCacheInstance(Paths.get(folderPath));
	}
}
