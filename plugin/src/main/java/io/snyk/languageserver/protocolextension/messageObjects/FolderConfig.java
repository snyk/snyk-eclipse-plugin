package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.List;

import com.google.gson.annotations.SerializedName;

public class FolderConfig {

    @SerializedName("folderPath")
    private String folderPath;

    @SerializedName("baseBranch")
    private String baseBranch;

    @SerializedName("localBranches")
    private List<String> localBranches;

    @SerializedName("additionalParameters")
    private List<String> additionalParameters;

    public FolderConfig(String folderPath, String baseBranch, List<String> localBranches, List<String> additionalParameters) {
        this.folderPath = folderPath;
        this.baseBranch = baseBranch;
        this.localBranches = localBranches != null ? localBranches : List.of();
        this.additionalParameters = additionalParameters != null ? additionalParameters : List.of();
    }

    public String getFolderPath() {
        return folderPath;
    }

    public void setFolderPath(String folderPath) {
        this.folderPath = folderPath;
    }

    public String getBaseBranch() {
        return baseBranch;
    }

    public void setBaseBranch(String baseBranch) {
        this.baseBranch = baseBranch;
    }

    public List<String> getLocalBranches() {
        return localBranches;
    }

    public void setLocalBranches(List<String> localBranches) {
        this.localBranches = localBranches != null ? localBranches : List.of();
    }

    public List<String> getAdditionalParameters() {
        return additionalParameters;
    }

    public void setAdditionalParameters(List<String> additionalParameters) {
        this.additionalParameters = additionalParameters != null ? additionalParameters : List.of();
    }
}