package io.snyk.languageserver.protocolextension.messageObjects;

public class LsSdk {
	private String type;
	private String path;
	
	public LsSdk(String type, String path) {
		this.type = type;
		this.path = path;
	}
	
	public String getPath() {
		return path;
	}
	public void setPath(String path) {
		this.path = path;
	}
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
}
