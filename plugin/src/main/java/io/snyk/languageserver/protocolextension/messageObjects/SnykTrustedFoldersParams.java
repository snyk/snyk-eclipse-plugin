package io.snyk.languageserver.protocolextension.messageObjects;

public class SnykTrustedFoldersParams {
  private String[] trustedFolders;

  public String[] getTrustedFolders() {
    return trustedFolders.clone();
  }

  public void setTrustedFolders(String... trustedFolders) {
    this.trustedFolders = trustedFolders.clone();
  }
}
