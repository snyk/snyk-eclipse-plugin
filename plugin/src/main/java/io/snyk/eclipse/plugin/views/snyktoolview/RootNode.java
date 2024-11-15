package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;

import io.snyk.eclipse.plugin.utils.ResourceUtils;

public class RootNode extends BaseTreeNode {
	public RootNode() {
		super("");
		reset();
	}

	public void reset() {
		var workspace = ResourcesPlugin.getWorkspace();
		IProject[] allProjects = workspace.getRoot().getProjects();
		List<IProject> openProjects = Arrays.stream(allProjects).filter(IProject::isOpen).collect(Collectors.toList());

		for (IProject project : openProjects) {
			Path path = ResourceUtils.getFullPath(project);
			var contentRoot = new ContentRootNode(project.getName(), path);
			this.addChild(contentRoot);
		}

	}
}
