package io.snyk.languageserver.protocolextension.messageObjects;

public class TreeViewParams {
	private String treeViewHtml;
	private int totalIssues;

	public String getTreeViewHtml() {
		return treeViewHtml;
	}

	public void setTreeViewHtml(String treeViewHtml) {
		this.treeViewHtml = treeViewHtml;
	}

	public int getTotalIssues() {
		return totalIssues;
	}

	public void setTotalIssues(int totalIssues) {
		this.totalIssues = totalIssues;
	}
}
