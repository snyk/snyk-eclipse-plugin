package io.snyk.languageserver.protocolextension.messageObjects;

public class SnykIsAvailableCliParams {
  private String cliPath;

  public String getCliPath() {
    return cliPath;
  }

  public void setCliPath(String cliPath) {
    this.cliPath = cliPath;
  }
}
