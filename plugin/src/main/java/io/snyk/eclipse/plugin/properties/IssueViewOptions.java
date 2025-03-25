package io.snyk.eclipse.plugin.properties;

public class IssueViewOptions {

	private final boolean openIssues;
	private final boolean ignoredIssues;

	public IssueViewOptions(boolean openIssues, boolean ignoredIssues) {
		this.openIssues = openIssues;
		this.ignoredIssues = ignoredIssues;
	}

	public boolean isShowingOpenIssues() {
		return this.openIssues;
	}

	public boolean isShowingIgnoredIssues() {
		return this.ignoredIssues;
	}

}
