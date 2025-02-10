package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.ArrayList;
import java.util.List;

import com.google.gson.annotations.SerializedName;

public class FolderConfig {

    @SerializedName("folderPath")
    private String folderPath;

    @SerializedName("baseBranch")
    private String baseBranch = "";

    @SerializedName("localBranches")
    private List<String> localBranches = new ArrayList<>();

    @SerializedName("additionalParameters")
    private List<String> additionalParameters = new ArrayList<>();
    
    @SerializedName("referenceFolderPath") 
    private String referenceFolderPath = "";
    	    
    @SerializedName("scanCommandConfig") 
    private ScanCommandConfig scanCommandConfig;

    public FolderConfig(String folderPath) {
        this.folderPath = folderPath;
    }

    public String getFolderPath() {
        return folderPath;
    }

    public void setFolderPath(String folderPath) {
        this.folderPath = folderPath;
    }

    public String getBaseBranch() {
    	if (baseBranch == null) return "";
        return baseBranch;
    }

    public void setBaseBranch(String baseBranch) {
        this.baseBranch = baseBranch;
    }

    public List<String> getLocalBranches() {
    	if (localBranches == null) return new ArrayList<>();
        return localBranches;
    }

    public void setLocalBranches(List<String> localBranches) {
        this.localBranches = localBranches != null ? localBranches : List.of();
    }

    public List<String> getAdditionalParameters() {
		if (additionalParameters == null) return new ArrayList<>();
        return additionalParameters;
    }

    public void setAdditionalParameters(List<String> additionalParameters) {
        this.additionalParameters = additionalParameters != null ? additionalParameters : List.of();
    }

	public String getReferenceFolderPath() {
		if (referenceFolderPath == null) return "";
		return referenceFolderPath;
	}

	public void setReferenceFolderPath(String referenceFolderPath) {
		this.referenceFolderPath = referenceFolderPath;
	}

	public ScanCommandConfig getScanCommandConfig() {
		return scanCommandConfig;
	}

	public void setScanCommandConfig(ScanCommandConfig scanCommandConfig) {
		this.scanCommandConfig = scanCommandConfig;
	}
}