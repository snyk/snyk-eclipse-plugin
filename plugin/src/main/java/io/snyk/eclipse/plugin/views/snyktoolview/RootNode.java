package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Path;
import java.util.List;

import org.eclipse.core.resources.IProject;

import io.snyk.eclipse.plugin.utils.ResourceUtils;

public class RootNode extends BaseTreeNode {
	public RootNode() {
		super("");
		reset(); // NOPMD by bdoetsch on 3/12/25, 11:48 AM
	}

	@Override
	public void reset() {
		super.reset();
		List<IProject> openProjects = ResourceUtils.getAccessibleTopLevelProjects();

		if (openProjects.isEmpty()) {
			var contentRoot = new ContentRootNode("No projects in workspace to scan", "",  null);
			this.addChild(contentRoot);
			return;
		}

		for (IProject project : openProjects) {
			Path path = ResourceUtils.getFullPath(project);
			BaseTreeNode contentRoot = new ContentRootNode(project.getName(), "", path); // NOPMD by bdoetsch on 3/12/25, 11:48 AM
			this.addChild(contentRoot);
		}
	}
}
