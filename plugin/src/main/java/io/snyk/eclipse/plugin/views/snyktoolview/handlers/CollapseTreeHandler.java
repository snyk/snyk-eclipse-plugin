package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.utils.SnykMessageDialog;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView;

public class CollapseTreeHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		String commandId = event.getCommand().getId();

		switch (commandId) {
		case "io.snyk.eclipse.plugin.commands.TreeCollapse":
			getView().getTreeViewer().collapseAll();
			break;
		case "io.snyk.eclipse.plugin.commands.TreeExpand":
			getView().getTreeViewer().expandAll();
			break;
		}

		return null;
	}

	public ISnykToolView getView() {

		IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		ISnykToolView toolView;
		try {
			toolView = (ISnykToolView) activePage.showView(SnykToolView.ID);
		} catch (PartInitException e) {
			SnykLogger.logError(e);
			return null;
		}
		return toolView;

	}
}