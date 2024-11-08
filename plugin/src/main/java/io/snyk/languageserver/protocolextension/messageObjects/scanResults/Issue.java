package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

public class Issue {
	public Issue() {
		
	}
	
	private String id;
    private String title;
    private String severity;
    private String filePath;
    private boolean isIgnored;
    private Range range;
    private boolean isNew;
    private IgnoreDetails ignoreDetails;
    private AdditionalData additionalData;
    private String product;
    
    public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}
	public String getSeverity() {
		return severity;
	}
	public void setSeverity(String severity) {
		this.severity = severity;
	}
	public String getFilePath() {
		return filePath;
	}
	public void setFilePath(String filePath) {
		this.filePath = filePath;
	}
	public boolean isIgnored() {
		return isIgnored;
	}
	public void setIgnored(boolean isIgnored) {
		this.isIgnored = isIgnored;
	}
	public boolean isNew() {
		return isNew;
	}
	public void setNew(boolean isNew) {
		this.isNew = isNew;
	}
	public IgnoreDetails getIgnoreDetails() {
		return ignoreDetails;
	}
	public void setIgnoreDetails(IgnoreDetails ignoreDetails) {
		this.ignoreDetails = ignoreDetails;
	}
	public AdditionalData getAdditionalData() {
		return additionalData;
	}
	public void setAdditionalData(AdditionalData additionalData) {
		this.additionalData = additionalData;
	}
	public String getProduct() {
		return product;
	}
	public void setProduct(String product) {
		this.product = product;
	}
	
    public String getDisplayTitle() {
        if (title == null || title.isEmpty()) {
            return this.additionalData != null ? this.additionalData.getMessage() : null;
        }
        return title;
    }

    public String getDisplayTitleWithLineNumber() {
        String lineNumber = (this.range != null && this.range.getEnd() != null) ? String.valueOf(this.range.getEnd().getLine()) : "unknown";
        String line = "line " + lineNumber + ": " + getDisplayTitle();
        if (this.additionalData != null && this.additionalData.isHasAIFix()) {
            line = "⚡" + line;
        }
        return line;
    }

    public String getPackageNameTitle() {
        if (this.additionalData != null) {
            return this.additionalData.getPackageName() + "@" + this.additionalData.getVersion() + ": " + title;
        }
        return title;
    }	
}
