package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

public class IgnoreDetails {
	public IgnoreDetails() {
		
	}
	
	private String category;
    private String reason;
    private String expiration;
    private String ignoredOn;
    private String ignoredBy;
    
    public String getCategory() {
		return category;
	}
	public void setCategory(String category) {
		this.category = category;
	}
	public String getReason() {
		return reason;
	}
	public void setReason(String reason) {
		this.reason = reason;
	}
	public String getExpiration() {
		return expiration;
	}
	public void setExpiration(String expiration) {
		this.expiration = expiration;
	}
	public String getIgnoredOn() {
		return ignoredOn;
	}
	public void setIgnoredOn(String ignoredOn) {
		this.ignoredOn = ignoredOn;
	}
	public String getIgnoredBy() {
		return ignoredBy;
	}
	public void setIgnoredBy(String ignoredBy) {
		this.ignoredBy = ignoredBy;
	}
}
