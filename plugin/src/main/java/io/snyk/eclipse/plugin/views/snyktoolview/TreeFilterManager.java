package io.snyk.eclipse.plugin.views.snyktoolview;

import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_QUALITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_SECURITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_IAC;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_OPEN_SOURCE;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_CRITICAL;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_HIGH;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_LOW;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_MEDIUM;

import java.util.function.Predicate;

import org.eclipse.jface.viewers.TreeViewer;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.FixableFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.ProductFilter;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.SeverityFilter;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class TreeFilterManager {

	private TreeViewer treeViewer;
	private TreeViewerFilter filter;
	private static TreeFilterManager filterManager;

	public synchronized static TreeFilterManager getInstance() {
		if (filterManager != null) {
			return filterManager;
		}
		filterManager = new TreeFilterManager(SnykStartup.getView().getTreeViewer());
		filterManager.reset();
		return filterManager;
	}

	private void setupFilters(TreeFilterManager tfm) {
		// Severity filters
		new SeverityFilter(tfm, FILTER_SHOW_CRITICAL).applyFilter();
		new SeverityFilter(tfm, FILTER_SHOW_HIGH).applyFilter();
		new SeverityFilter(tfm, FILTER_SHOW_MEDIUM).applyFilter();
		new SeverityFilter(tfm, FILTER_SHOW_LOW).applyFilter();
		
		// Product filters
		new ProductFilter(tfm, ACTIVATE_SNYK_OPEN_SOURCE).applyFilter();
		new ProductFilter(tfm, ACTIVATE_SNYK_IAC).applyFilter();
		new ProductFilter(tfm, ACTIVATE_SNYK_CODE_SECURITY).applyFilter();
		new ProductFilter(tfm, ACTIVATE_SNYK_CODE_QUALITY).applyFilter();

		// Fix
		new FixableFilter(tfm).applyFilter();
	}
	
	public void reset() {
		filter = new TreeViewerFilter();
		treeViewer.resetFilters();
		treeViewer.setFilters(filter);
		setupFilters(this);
	}

	private TreeFilterManager(TreeViewer treeViewer) {
		this.treeViewer = treeViewer;
	}

	public void addTreeFilter(String filterName, Predicate<? super Issue> filterPredicate) {
		filter.putFilterPredicate(filterName, filterPredicate);
		updateTree(treeViewer);
	}

	public void removeTreeFilter(String filterName) {
		filter.removeFilterPredicate(filterName);
		updateTree(treeViewer);
	}

	public void removeTreeFilters() {
		treeViewer.resetFilters();
		updateTree(treeViewer);
	}

	private void updateTree(TreeViewer treeView) {
		treeView.refresh();
		treeView.expandAll();
	}

}
