package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.jface.resource.ImageDescriptor;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.utils.SnykIcons;
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
		switch (getIssue().severity()) {
		case ProductConstants.SEVERITY_CRITICAL:
			return SnykIcons.getImageDescriptor(SnykIcons.SEVERITY_CRITICAL_ID);
		case ProductConstants.SEVERITY_HIGH:
			return SnykIcons.getImageDescriptor(SnykIcons.SEVERITY_HIGH_ID);
		case ProductConstants.SEVERITY_MEDIUM:
			return SnykIcons.getImageDescriptor(SnykIcons.SEVERITY_MEDIUM_ID);
		case ProductConstants.SEVERITY_LOW:
			return SnykIcons.getImageDescriptor(SnykIcons.SEVERITY_LOW_ID);
		default:
			return super.getImageDescriptor();
		}
	}

	public Issue getIssue() {
		return issue;
	}

	public final void setIssue(Issue issue) {
		this.issue = issue;
	}

}
