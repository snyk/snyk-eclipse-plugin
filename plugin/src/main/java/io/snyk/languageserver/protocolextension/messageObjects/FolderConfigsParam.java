package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.List;

public class FolderConfigsParam {

	private List<FolderConfig> folderConfigs;

	public FolderConfigsParam(List<FolderConfig> folderConfigs) {
		this.folderConfigs = folderConfigs;
	}

	public List<FolderConfig> getFolderConfigs() {
		return folderConfigs;
	}

	public void setFolderConfigs(List<FolderConfig> folderConfigs) {
		this.folderConfigs = folderConfigs;
	}
}
