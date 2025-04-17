package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

import org.eclipse.lsp4j.Position;

public record Issue(String id, String title, String severity, String filePath, Range range, boolean isIgnored,
		boolean isNew, String filterableIssueType, IgnoreDetails ignoreDetails, AdditionalData additionalData) {
	public String getDisplayTitle() {
		if (title == null || title.isEmpty()) {
			return additionalData != null ? additionalData.message() : null;
		}
		StringBuilder displayTitleBuilder = new StringBuilder(title);
		if (isIgnored()) {
			displayTitleBuilder.insert(0, " [ Ignored ] ");
		}

		if (hasFix()) {
			displayTitleBuilder.insert(0, " ⚡");
		}

		return displayTitleBuilder.toString();
	}

	public String getDisplayTitleWithLineNumber() {
		String lineNumber = (range != null && range.end() != null) ? String.valueOf(range.end().line()) : "unknown";
		String line = "line " + lineNumber + ": " + getDisplayTitle();
		if (hasFix()) {
			line = "⚡" + line;
		}
		return line;
	}

	public String getPackageNameTitle() {
		if (additionalData != null) {
			return additionalData.packageName() + "@" + additionalData.version() + ": " + title;
		}
		return title;
	}

	public org.eclipse.lsp4j.Range getLSP4JRange() {
		LineRange localStart = range.start();
		var start = new Position(localStart.line(), localStart.character());
		LineRange localEnd = range.end();
		var end = new Position(localEnd.line(), localEnd.character());
		return new org.eclipse.lsp4j.Range(start, end);
	}

	public Boolean hasFix() {
		if (additionalData == null) {
			return false;
		}
		return additionalData.hasAIFix() || additionalData.isUpgradable();
	}
}
