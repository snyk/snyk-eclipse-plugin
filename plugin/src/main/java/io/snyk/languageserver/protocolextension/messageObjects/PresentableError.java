// ABOUTME: This class represents structured error information from the Snyk Language Server
package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;

public class PresentableError {
    @JsonProperty("code")
    private Integer code;

    @JsonProperty("error")
    private String error;

    @JsonProperty("path")
    private String path;

    @JsonProperty("command")
    private String command;

    // PresentableError specific fields
    @JsonProperty("showNotification")
    private boolean showNotification;

    @JsonProperty("treeNodeSuffix")
    private String treeNodeSuffix;

    public PresentableError() {
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getError() {
        return error;
    }

    public void setError(String error) {
        this.error = error;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getCommand() {
        return command;
    }

    public void setCommand(String command) {
        this.command = command;
    }

    public boolean isShowNotification() {
        return showNotification;
    }

    public void setShowNotification(boolean showNotification) {
        this.showNotification = showNotification;
    }

    public String getTreeNodeSuffix() {
        return treeNodeSuffix;
    }

    public void setTreeNodeSuffix(String treeNodeSuffix) {
        this.treeNodeSuffix = treeNodeSuffix;
    }

    @Override
    public String toString() {
        return "PresentableError [code=" + code + ", error=" + error + ", path=" + path + ", command=" + command
                + ", showNotification=" + showNotification + ", treeNodeSuffix=" + treeNodeSuffix + "]";
    }

    @Override
    public int hashCode() {
        return Objects.hash(code, error, path, command, showNotification, treeNodeSuffix);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        PresentableError other = (PresentableError) obj;
        return Objects.equals(code, other.code) && Objects.equals(error, other.error)
                && Objects.equals(path, other.path) && Objects.equals(command, other.command)
                && showNotification == other.showNotification && Objects.equals(treeNodeSuffix, other.treeNodeSuffix);
    }
}
