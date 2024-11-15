package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.jface.resource.ImageDescriptor;

import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class IssueTreeNode extends BaseTreeNode {
	private Issue issue;

	public IssueTreeNode(Issue issue) {
		super(issue);
		this.setIssue(issue);
	}

	@Override
	public String getText() {
		return getIssue().getDisplayTitle();
	}

	@Override
	public String getDetails() {
		return getIssue().additionalData().customUIContent();
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		// TODO Implement me to show fancy severity icons here
		return super.getImageDescriptor();
	}

	public Issue getIssue() {
		return issue;
	}

	public void setIssue(Issue issue) {
		this.issue = issue;
	}
	
	
}
