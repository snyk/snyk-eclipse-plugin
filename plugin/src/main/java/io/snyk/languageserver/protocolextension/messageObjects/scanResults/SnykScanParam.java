package io.snyk.languageserver.protocolextension.messageObjects.scanResults;

public class SnykScanParam {
	public SnykScanParam() {
		
	}
	
	private String status;
	private String product;
	private String folderPath;
	private String errorMessage;
	private Issue[] issues;
	
	public String getStatus() {
		return status;
	}
	public void setStatus(String status) {
		this.status = status;
	}
	public String getProduct() {
		return product;
	}
	public void setProduct(String product) {
		this.product = product;
	}
	public String getFolderPath() {
		return folderPath;
	}
	public void setFolderPath(String folderPath) {
		this.folderPath = folderPath;
	}
	public String getErrorMessage() {
		return errorMessage;
	}
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}
	public Issue[] getIssues() {
		return issues;
	}
	public void setIssues(Issue[] issues) {
		this.issues = issues;
	}
}

