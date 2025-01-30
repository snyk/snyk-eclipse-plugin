package io.snyk.languageserver.protocolextension.messageObjects;

import java.util.Arrays;

public class SnykTrustedFoldersParams {
  private String[] trustedFolders;

  public String[] getTrustedFolders() {
    return Arrays.copyOf(trustedFolders, trustedFolders.length);
  }

  public void setTrustedFolders(String... trustedFolders) {
    this.trustedFolders = trustedFolders.clone();
  }
}
