package io.snyk.eclipse.plugin.wizards;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class SnykWizardModel {
  protected String path;
  protected String languageServer;
  protected String cli;
  protected String manageDependenciesEnabled;
  
  protected String endpoint;
  protected String unknownCerts;
  
  protected String openSourceEnabled;
  protected String codeEnabled;
  protected String iacEnabled;
  
  protected String organization;
  protected String additionalParameters;
  protected String additionalEnvironment;
  protected String telemetryEnabled;
  protected String errorReportsEnabled;

  public SnykWizardModel() {
    this.path = Preferences.getInstance().getPref(Preferences.PATH_KEY);
    this.languageServer = Preferences.getInstance().getPref(Preferences.LS_BINARY_KEY);
    this.cli = Preferences.getInstance().getPref(Preferences.CLI_PATH);
    this.manageDependenciesEnabled = Preferences.getInstance().getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY);
    
    this.endpoint = Preferences.getInstance().getPref(Preferences.ENDPOINT_KEY);
    this.unknownCerts = Preferences.getInstance().getPref(Preferences.INSECURE_KEY);
    
    this.openSourceEnabled = Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE);
    this.codeEnabled = Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_CODE);
    this.iacEnabled = Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_IAC);
    
    this.organization = Preferences.getInstance().getPref(Preferences.ORGANIZATION_KEY);
    this.additionalParameters = Preferences.getInstance().getPref(Preferences.ADDITIONAL_PARAMETERS);
    this.additionalEnvironment = Preferences.getInstance().getPref(Preferences.ADDITIONAL_ENVIRONMENT);
    this.telemetryEnabled = Preferences.getInstance().getPref(Preferences.ENABLE_TELEMETRY);
    this.errorReportsEnabled = Preferences.getInstance().getPref(Preferences.SEND_ERROR_REPORTS);
  }

  public void resetPreferences() {
    Preferences.getInstance().store(Preferences.PATH_KEY, path);
    Preferences.getInstance().store(Preferences.LS_BINARY_KEY, languageServer);
    Preferences.getInstance().store(Preferences.CLI_PATH, cli);
    Preferences.getInstance().store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, manageDependenciesEnabled);
    
    Preferences.getInstance().store(Preferences.ENDPOINT_KEY, endpoint);
    Preferences.getInstance().store(Preferences.INSECURE_KEY, unknownCerts);
    
    Preferences.getInstance().store(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, openSourceEnabled);
    Preferences.getInstance().store(Preferences.ACTIVATE_SNYK_CODE, codeEnabled);
    Preferences.getInstance().store(Preferences.ACTIVATE_SNYK_IAC, iacEnabled);
    
    Preferences.getInstance().store(Preferences.ORGANIZATION_KEY, organization);
    Preferences.getInstance().store(Preferences.ADDITIONAL_PARAMETERS, additionalParameters);
    Preferences.getInstance().store(Preferences.ADDITIONAL_ENVIRONMENT, additionalEnvironment);
    Preferences.getInstance().store(Preferences.ENABLE_TELEMETRY, telemetryEnabled);
    Preferences.getInstance().store(Preferences.SEND_ERROR_REPORTS, errorReportsEnabled);
  }
}
