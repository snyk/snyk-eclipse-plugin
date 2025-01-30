package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import io.snyk.eclipse.plugin.SnykStartup;

public class CollapseTreeHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		String commandId = event.getCommand().getId();

		String treeCollapse = "io.snyk.eclipse.plugin.commands.TreeCollapse";
		String treeExpand = "io.snyk.eclipse.plugin.commands.TreeExpand";

		if (commandId.equals(treeCollapse)) {
			SnykStartup.getView().getTreeViewer().collapseAll();
		} else if (commandId.equals(treeExpand)) {
			SnykStartup.getView().getTreeViewer().expandAll();
		}

		return null;
	}
}
