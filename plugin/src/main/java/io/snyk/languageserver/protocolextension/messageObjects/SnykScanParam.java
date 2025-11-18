package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SnykScanParam {
    public SnykScanParam() {

    }

    private String status;
    private String product;
    private String folderPath;

    @JsonProperty("presentableError")
    private PresentableError presentableError;

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
    public PresentableError getPresentableError() {
        return presentableError;
    }
    public void setPresentableError(PresentableError presentableError) {
        this.presentableError = presentableError;
    }

    @Override
    public String toString() {
        return "SnykScanParam [status=" + status + ", product=" + product + ", folderPath=" + folderPath
                + ", presentableError=" + presentableError + "]";
    }

    @Override
    public int hashCode() {
        return Objects.hash(presentableError, folderPath, product, status);
    }
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SnykScanParam other = (SnykScanParam) obj;
        return Objects.equals(presentableError, other.presentableError) && Objects.equals(folderPath, other.folderPath)
                && Objects.equals(product, other.product) && Objects.equals(status, other.status);
    }
}

