package io.snyk.eclipse.plugin.properties.preferences;

import io.snyk.eclipse.plugin.preferences.Preferences;

public abstract class PreferencesUtils {
  public static void setPreferences(Preferences p) {
    Preferences.setCurrentPreferences(p);
  }

  public static void reset() {
    Preferences.setCurrentPreferences(null);
  }
}
