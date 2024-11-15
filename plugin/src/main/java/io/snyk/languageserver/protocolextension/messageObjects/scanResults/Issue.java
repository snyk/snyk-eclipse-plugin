package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

public record Issue(
	    String id,
	    String title,
	    String severity,
	    String filePath,
	    Range range,
	    boolean isIgnored,
	    boolean isNew,
	    IgnoreDetails ignoreDetails,
	    AdditionalData additionalData,
	    String product
	) {
	    public String getDisplayTitle() {
	        if (title == null || title.isEmpty()) {
	            return additionalData != null ? additionalData.message() : null;
	        }
	        return title;
	    }

	    public String getDisplayTitleWithLineNumber() {
	        String lineNumber = (range != null && range.end() != null)
	            ? String.valueOf(range.end().line())
	            : "unknown";
	        String line = "line " + lineNumber + ": " + getDisplayTitle();
	        if (additionalData != null && additionalData.hasAIFix()) {
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
	}
