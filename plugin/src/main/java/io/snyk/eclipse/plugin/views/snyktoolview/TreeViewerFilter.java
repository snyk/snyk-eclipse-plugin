package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

public class TreeViewerFilter extends ViewerFilter {
	private List<String> searchStrings;
	private boolean matchAll; // If true, all filters must match; if false, any filter can match

	public TreeViewerFilter() {
		this.searchStrings = new ArrayList<>();
		this.matchAll = true; // Default to matching all filters
	}

	public void addSearchText(String s) {
		if (s != null && !s.isEmpty()) {
			this.searchStrings.add(".*" + s.toLowerCase() + ".*");
		}
	}

	public void clearSearchText() {
		this.searchStrings.clear();
	}

	public void setMatchAll(boolean matchAll) {
		this.matchAll = matchAll;
	}

	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if (searchStrings.isEmpty()) {
			return true;
		}

		TreeViewer treeViewer = (TreeViewer) viewer;
		String label = ((ILabelProvider) treeViewer.getLabelProvider()).getText(element);

		if (label == null) {
			return false;
		}

		String labelLower = label.toLowerCase();

		if (matchAll) {
			return searchStrings.stream().allMatch(s -> labelLower.matches(s));
		} else {
			return searchStrings.stream().anyMatch(s -> labelLower.matches(s));
		}
	}
}
