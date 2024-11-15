package io.snyk.eclipse.plugin.views.snyktoolview;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class IssueTreeNode extends BaseTreeNode {
	private Issue issue;

	public IssueTreeNode(Issue issue) {
		super(issue);
		this.issue = issue;
	}

	@Override
	public String getText() {
		return issue.getDisplayTitle();
	}

	@Override
	public String getDetails() {
		return issue.additionalData().customUIContent();
	}
}
