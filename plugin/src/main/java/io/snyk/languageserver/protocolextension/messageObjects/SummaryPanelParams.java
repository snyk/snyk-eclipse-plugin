package io.snyk.languageserver.protocolextension.messageObjects;

public class SummaryPanelParams {
	  private String scanSummary;

	  public String getSummary() {
	    return scanSummary;
	  }

	  public void setSummary(String scanSummary) {
	    this.scanSummary = scanSummary;
	  }
}
