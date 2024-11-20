package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeViewerFilter;

public class SnykFilterFixableIssuesHandler extends BaseHandler implements IElementUpdater {

	public SnykFilterFixableIssuesHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		iconDisabled = SnykIcons.DISABLED;
		preferenceKey = Preferences.FILTER_FIXABLE_ISSUES;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		TreeViewer treeView = SnykStartup.getView().getTreeViewer();
		TreeViewerFilter filter = new TreeViewerFilter();

		filter.addSearchText("⚡"); // Ai Fix filter flash string

		// TODO remove these search strings and make sure we just filter out items of
		// the correct node type in our TreeViewerFilter.select() function.
		filter.addSearchText("Open Source");
		filter.addSearchText("Code");
		filter.addSearchText("Configuration");

		// TODO Add the correct filter strings here when we have items in the treeview.
		// The fixable states open source issues can have are one of: Fixable,
		// Partially Fixable, No Supported Fix, Any Fixability Level
		// filter.addSearchText(""); // OSS Upgradable filter string

		filter.setMatchAll(false);

		boolean booleanPref = Preferences.getInstance().getBooleanPref(preferenceKey);
		if (booleanPref) {
			applyTreeFilter(treeView, filter);
		} else {
			removeTreeFilter(treeView);
		}

		// Update the application state for the preference.
		Preferences.getInstance().store(preferenceKey, Boolean.valueOf(!booleanPref).toString());

		// Lastly update the UI.
		String commandId = event.getCommand().getId();
		ICommandService commandService = PlatformUI.getWorkbench().getService(ICommandService.class);
		if (commandService != null) {
			commandService.refreshElements(commandId, null);
		}

		return null;
	}

	private void removeTreeFilter(TreeViewer treeView) {
		treeView.resetFilters();
		updateTree(treeView);
	}

	private void applyTreeFilter(TreeViewer treeView, TreeViewerFilter filter) {
		treeView.addFilter(filter);
		updateTree(treeView);
	}

	private void updateTree(TreeViewer treeView) {
		treeView.getControl().setRedraw(false);
		treeView.refresh();
		treeView.getControl().setRedraw(true);
	}

}
