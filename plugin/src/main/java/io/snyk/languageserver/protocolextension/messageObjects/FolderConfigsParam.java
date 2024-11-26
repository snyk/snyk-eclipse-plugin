package io.snyk.languageserver.protocolextension.messageObjects;

import com.google.gson.annotations.SerializedName;
import java.util.List;

public class FolderConfigsParam {

    @SerializedName("folderConfigs")
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