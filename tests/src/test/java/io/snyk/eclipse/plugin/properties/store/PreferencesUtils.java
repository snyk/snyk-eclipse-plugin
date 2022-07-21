package io.snyk.eclipse.plugin.properties.store;

public abstract class PreferencesUtils {
  public static void setPreferences(Preferences p) {
    Preferences.CURRENT_PREFERENCES = p;
  }
}
