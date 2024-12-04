package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Path;
import java.util.List;

import org.eclipse.core.resources.IProject;

import io.snyk.eclipse.plugin.utils.ResourceUtils;

public class RootNode extends BaseTreeNode {
	public RootNode() {
		super("");
		reset();
	}

	public void reset() {
		super.reset();
		List<IProject> openProjects = ResourceUtils.getAccessibleTopLevelProjects();

		for (IProject project : openProjects) {
			Path path = ResourceUtils.getFullPath(project);
			var contentRoot = new ContentRootNode(project.getName(), path);
			this.addChild(contentRoot);
		}
	}
}
