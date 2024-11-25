package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.function.Predicate;

import org.eclipse.jface.viewers.TreeViewer;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class TreeFilterManager {

	private TreeViewer treeView;
	private TreeViewerFilter filter;
	private static TreeFilterManager filterManager;

	public synchronized static TreeFilterManager getInstance() {
		if (filterManager != null) {
			return filterManager;
		}
		filterManager = new TreeFilterManager();
		return filterManager;
	}

	private TreeFilterManager() {
		treeView = SnykStartup.getView().getTreeViewer();
		filter = new TreeViewerFilter();
	}

	public void addTreeFilter(String filterName, Predicate<? super Issue> filterPredicate) {
		filter.setFilterPredicate(filterName, filterPredicate);
		treeView.addFilter(filter);
		updateTree(treeView);
	}

	public void removeTreeFilter(String filterName) {
		filter.removeFilterPredicate(filterName);
		treeView.addFilter(filter);
		updateTree(treeView);
	}

	public void removeTreeFilters() {
		treeView.resetFilters();
		updateTree(treeView);
	}

	private void updateTree(TreeViewer treeView) {
		treeView.getControl().setRedraw(false);
		treeView.refresh();
		treeView.getControl().setRedraw(true);
	}

}
