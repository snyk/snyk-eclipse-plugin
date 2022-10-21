package io.snyk.eclipse.plugin.wizards;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class SnykWizardModel {
  private String initialEndpoint;
  private String initialUnknownCerts;

  public SnykWizardModel() {
    this.initialEndpoint = Preferences.getInstance().getPref(Preferences.ENDPOINT_KEY);
    this.initialUnknownCerts = Preferences.getInstance().getPref(Preferences.INSECURE_KEY);
  }

  public void resetPreferences() {
    Preferences.getInstance().store(Preferences.ENDPOINT_KEY, initialEndpoint);
    Preferences.getInstance().store(Preferences.INSECURE_KEY, initialUnknownCerts);
  }
}
