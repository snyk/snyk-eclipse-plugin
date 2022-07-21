package io.snyk.eclipse.plugin.properties.preferences;

public abstract class PreferencesUtils {
  public static void setPreferences(Preferences p) {
    Preferences.CURRENT_PREFERENCES = p;
  }
}
