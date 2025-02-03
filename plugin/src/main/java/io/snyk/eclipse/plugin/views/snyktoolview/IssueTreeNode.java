package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.jface.resource.ImageDescriptor;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.utils.SnykIcons;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class IssueTreeNode extends BaseTreeNode {
	private Issue issue;

	public IssueTreeNode(Issue issue) {
		super(issue);
		this.issue = issue;
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
		switch (getIssue().severity()) {
		case ProductConstants.SEVERITY_CRITICAL:
			return SnykIcons.SEVERITY_CRITICAL;
		case ProductConstants.SEVERITY_HIGH:
			return SnykIcons.SEVERITY_HIGH;
		case ProductConstants.SEVERITY_MEDIUM:
			return SnykIcons.SEVERITY_MEDIUM;
		case ProductConstants.SEVERITY_LOW:
			return SnykIcons.SEVERITY_LOW;
		default:
			return super.getImageDescriptor();
		}
	}

	public Issue getIssue() {
		return issue;
	}

	public void setIssue(Issue issue) {
		this.issue = issue;
	}

}
