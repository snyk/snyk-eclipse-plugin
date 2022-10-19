package io.snyk.eclipse.plugin.wizards;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class SnykWizardModel {
  protected String initialManageDependenciesEnabled;
  protected String initialPath;
  protected String initialLanguageServer;
  protected String initialCli;
  
  protected String initialEndpoint;
  protected String initialUnknownCerts;
  
  protected String initialOpenSourceEnabled;
  protected String initialCodeEnabled;
  protected String initialIacEnabled;
  
  protected String initialOrganization;
  protected String initialAdditionalParameters;
  protected String initialAdditionalEnvironment;
  protected String initialTelemetryEnabled;
  protected String initialErrorReportsEnabled;

  public SnykWizardModel() {
    this.initialManageDependenciesEnabled = Preferences.getInstance().getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY);
    this.initialPath = Preferences.getInstance().getPref(Preferences.PATH_KEY);
    this.initialLanguageServer = Preferences.getInstance().getPref(Preferences.LS_BINARY_KEY);
    this.initialCli = Preferences.getInstance().getPref(Preferences.CLI_PATH);
    
    this.initialEndpoint = Preferences.getInstance().getPref(Preferences.ENDPOINT_KEY);
    this.initialUnknownCerts = Preferences.getInstance().getPref(Preferences.INSECURE_KEY);
    
    this.initialOpenSourceEnabled = Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE);
    this.initialCodeEnabled = Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_CODE);
    this.initialIacEnabled = Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_IAC);
    
    this.initialOrganization = Preferences.getInstance().getPref(Preferences.ORGANIZATION_KEY);
    this.initialAdditionalParameters = Preferences.getInstance().getPref(Preferences.ADDITIONAL_PARAMETERS);
    this.initialAdditionalEnvironment = Preferences.getInstance().getPref(Preferences.ADDITIONAL_ENVIRONMENT);
    this.initialTelemetryEnabled = Preferences.getInstance().getPref(Preferences.ENABLE_TELEMETRY);
    this.initialErrorReportsEnabled = Preferences.getInstance().getPref(Preferences.SEND_ERROR_REPORTS);
  }

  public void resetPreferences() {
    Preferences.getInstance().store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, initialManageDependenciesEnabled);
    Preferences.getInstance().store(Preferences.PATH_KEY, initialPath);
    Preferences.getInstance().store(Preferences.LS_BINARY_KEY, initialLanguageServer);
    Preferences.getInstance().store(Preferences.CLI_PATH, initialCli);
    
    Preferences.getInstance().store(Preferences.ENDPOINT_KEY, initialEndpoint);
    Preferences.getInstance().store(Preferences.INSECURE_KEY, initialUnknownCerts);
    
    Preferences.getInstance().store(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, initialOpenSourceEnabled);
    Preferences.getInstance().store(Preferences.ACTIVATE_SNYK_CODE, initialCodeEnabled);
    Preferences.getInstance().store(Preferences.ACTIVATE_SNYK_IAC, initialIacEnabled);
    
    Preferences.getInstance().store(Preferences.ORGANIZATION_KEY, initialOrganization);
    Preferences.getInstance().store(Preferences.ADDITIONAL_PARAMETERS, initialAdditionalParameters);
    Preferences.getInstance().store(Preferences.ADDITIONAL_ENVIRONMENT, initialAdditionalEnvironment);
    Preferences.getInstance().store(Preferences.ENABLE_TELEMETRY, initialTelemetryEnabled);
    Preferences.getInstance().store(Preferences.SEND_ERROR_REPORTS, initialErrorReportsEnabled);
  }
}
