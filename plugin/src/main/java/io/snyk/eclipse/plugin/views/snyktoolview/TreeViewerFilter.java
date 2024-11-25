package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class TreeViewerFilter extends ViewerFilter {
	private Map<String, Predicate<? super Issue>> filters;

	public TreeViewerFilter() {
		filters = new ConcurrentHashMap<>();
	}

	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if (!(element instanceof IssueTreeNode) || !(viewer instanceof TreeViewer))
			return true;

		IssueTreeNode IssueTreeNode = (IssueTreeNode) element;
		var issue = IssueTreeNode.getIssue();
		if (issue == null) {
			return true;
		}

		for (var kv : this.filters.entrySet()) {
			var filter = kv.getValue();
			if (!filter.test(issue)) {
				return false;
			}
		}
		return true;
	}

	public void setFilterPredicate(String filterName, Predicate<? super Issue> predicate) {
		this.filters.put(filterName, predicate);
	}

	public void removeFilterPredicate(String filterName) {
		this.filters.remove(filterName);
	}

}
