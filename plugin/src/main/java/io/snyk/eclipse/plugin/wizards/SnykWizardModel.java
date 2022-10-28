package io.snyk.eclipse.plugin.wizards;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.LsConfigurationUpdater;

public class SnykWizardModel {
  private String initialEndpoint;
  private String initialUnknownCerts;
  private String initialAuthToken;

  public SnykWizardModel() {
    this.initialEndpoint = Preferences.getInstance().getPref(Preferences.ENDPOINT_KEY);
    this.initialUnknownCerts = Preferences.getInstance().getPref(Preferences.INSECURE_KEY);
    this.initialAuthToken = Preferences.getInstance().getPref(Preferences.AUTH_TOKEN_KEY);
  }

  public void resetPreferences() {
    Preferences.getInstance().store(Preferences.ENDPOINT_KEY, initialEndpoint);
    Preferences.getInstance().store(Preferences.INSECURE_KEY, initialUnknownCerts);
    Preferences.getInstance().store(Preferences.AUTH_TOKEN_KEY, initialAuthToken);
    
    new LsConfigurationUpdater().configurationChanged();
  }
}
