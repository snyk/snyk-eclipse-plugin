package io.snyk.languageserver.protocolextension.messageObjects;

public class ScanParams {	
	public ScanParams() {
		
	}
	
	String status;
	String product;
	String folderPath;
	
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
}
