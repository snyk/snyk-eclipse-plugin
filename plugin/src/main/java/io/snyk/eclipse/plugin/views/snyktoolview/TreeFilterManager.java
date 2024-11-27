package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.function.Predicate;

import org.eclipse.jface.viewers.TreeViewer;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.FixableFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.IgnoresFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.IgnoresOpenIssuesFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.OssFixableFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.SeverityCriticalFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.SeverityHighFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.SeverityLowFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.SeverityMediumFilter;
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

		setupFilters();
		return filterManager;
	}

	private static void setupFilters() {
		// Severity filters
		new SeverityCriticalFilter(TreeFilterManager.getInstance(), Preferences.getInstance(),
				Preferences.FILTER_CRITICAL).applyFilter();
		new SeverityHighFilter(TreeFilterManager.getInstance(), Preferences.getInstance(), Preferences.FILTER_HIGH)
				.applyFilter();
		new SeverityMediumFilter(TreeFilterManager.getInstance(), Preferences.getInstance(), Preferences.FILTER_MEDIUM)
				.applyFilter();
		new SeverityLowFilter(TreeFilterManager.getInstance(), Preferences.getInstance(), Preferences.FILTER_LOW)
				.applyFilter();

		// Ignores filters
		new IgnoresFilter(TreeFilterManager.getInstance(), Preferences.getInstance(),
				Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES).applyFilter();
		new IgnoresOpenIssuesFilter(TreeFilterManager.getInstance(), Preferences.getInstance(),
				Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES).applyFilter();

		// Fix
		new FixableFilter(TreeFilterManager.getInstance(), Preferences.getInstance(), Preferences.FILTER_FIXABLE_ISSUES)
				.applyFilter();
		new OssFixableFilter(TreeFilterManager.getInstance(), Preferences.getInstance(),
				Preferences.FILTER_OSS_FIXABLE_ISSUES).applyFilter();
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
		treeView.expandAll();
	}

}
