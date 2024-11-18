package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView;

public class snykFilterFixableIssuesHandler extends BaseHandler implements IElementUpdater {

	public snykFilterFixableIssuesHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		preferenceKey = Preferences.FILTER_FIXABLE_ISSUES;

		TreeViewerFilter filter = new TreeViewerFilter();

		// TODO Add the correct filter strings here when we have items in the treeview.
		// The fixable states a Ai Fix issues can have are one of: AI Fixable, Not AI
		// Fixable or Any AI Fix Status
		filter.addSearchText(""); // Ai Fix filter flash string

		// The fixable states open source issues can have are one of: Fixable,
		// Partially Fixable, No Supported Fix, Any Fixability Level
		filter.addSearchText(""); // OSS Upgradable filter string
		filter.setMatchAll(true);

		TreeViewer treeView = getView().getTreeViewer();
		treeView.addFilter(filter);

		treeView.getControl().setRedraw(false);
		treeView.refresh();
		treeView.getControl().setRedraw(true);

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
