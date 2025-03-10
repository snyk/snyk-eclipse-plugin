package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;

import org.eclipse.jface.viewers.ITreeContentProvider;
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
	    if (element instanceof FileTreeNode) {
	        return hasVisibleChildren((FileTreeNode) element, viewer);
	    }
	    
	    if (!(element instanceof IssueTreeNode) || !(viewer instanceof TreeViewer)) {
	        return true;
	    }

	    return isIssueVisible((IssueTreeNode) element);
	}

	private boolean hasVisibleChildren(FileTreeNode fileNode, Viewer viewer) {
	    Object[] children = ((ITreeContentProvider) ((TreeViewer) viewer).getContentProvider()).getChildren(fileNode);
	    for (Object child : children) {
	        if (child instanceof IssueTreeNode && isIssueVisible((IssueTreeNode) child)) {
	            return true;
	        }
	    }
	    return false;
	}

	private boolean isIssueVisible(IssueTreeNode issueNode) {
	    Issue issue = issueNode.getIssue();
	    if (issue == null) {
	        return true;
	    }

	    for (var filter : this.filters.values()) {
	        if (filter.test(issue)) {
	            return false;
	        }
	    }
	    return true;
	}

	public void putFilterPredicate(String filterName, Predicate<? super Issue> predicate) {
		this.filters.put(filterName, predicate);
	}

	public void removeFilterPredicate(String filterName) {
		this.filters.remove(filterName);
	}

}
