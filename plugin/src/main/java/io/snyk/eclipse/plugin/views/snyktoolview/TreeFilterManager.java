package io.snyk.eclipse.plugin.views.snyktoolview;

import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_FIXABLE_ISSUES;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_CRITICAL;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_HIGH;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_LOW;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_MEDIUM;

import java.util.function.Predicate;

import org.eclipse.jface.viewers.TreeViewer;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.FixableFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.IgnoresFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.IgnoresOpenIssuesFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.SeverityFilter;
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

	private static void setupFilters() {
		// Severity filters
		new SeverityFilter(FILTER_SHOW_CRITICAL).applyFilter();
		new SeverityFilter(FILTER_SHOW_HIGH).applyFilter();
		new SeverityFilter(FILTER_SHOW_MEDIUM).applyFilter();
		new SeverityFilter(FILTER_SHOW_LOW).applyFilter();

		// Ignores filters
		new IgnoresFilter(FILTER_IGNORES_SHOW_IGNORED_ISSUES).applyFilter();
		new IgnoresOpenIssuesFilter(FILTER_IGNORES_SHOW_OPEN_ISSUES).applyFilter();

		// Fix
		new FixableFilter(FILTER_FIXABLE_ISSUES).applyFilter();
	}
	
	public void reset() {
		treeView = SnykStartup.getView().getTreeViewer();
		filter = new TreeViewerFilter();
		setupFilters();
	}

	private TreeFilterManager() {
		reset();
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
		treeView.expandAll();
	}

}
