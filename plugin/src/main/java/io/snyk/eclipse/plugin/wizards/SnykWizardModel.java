package io.snyk.eclipse.plugin.wizards;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.LsConfigurationUpdater;

public class SnykWizardModel {
  private String initialEndpoint;
  private boolean initialUnknownCerts;
  private String initialAuthToken;

  public SnykWizardModel() {
    this.initialEndpoint = Preferences.getInstance().getPref(Preferences.ENDPOINT_KEY, "");
    this.initialUnknownCerts = Preferences.getInstance().isInsecure();
    this.initialAuthToken = Preferences.getInstance().getAuthToken();
  }

  public void resetPreferences() {
    Preferences.getInstance().store(Preferences.ENDPOINT_KEY, initialEndpoint);
    Preferences.getInstance().setIsInsecure(initialUnknownCerts);
    Preferences.getInstance().store(Preferences.AUTH_TOKEN_KEY, initialAuthToken);
    
    new LsConfigurationUpdater().configurationChanged();
  }
}
