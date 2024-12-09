package io.snyk.eclipse.plugin.properties.preferences;

import io.snyk.eclipse.plugin.preferences.Preferences;

public abstract class PreferencesUtils {
  public static void setPreferences(Preferences p) {
    Preferences.CURRENT_PREFERENCES = p;
  }

  public static void reset() {
    Preferences.CURRENT_PREFERENCES = null;
  }
}
