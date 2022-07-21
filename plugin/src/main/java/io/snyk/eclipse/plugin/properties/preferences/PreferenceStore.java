package io.snyk.eclipse.plugin.properties.preferences;

import org.eclipse.jface.preference.IPreferenceStore;

public interface PreferenceStore {
  boolean getBoolean(String key, boolean defaultValue);
  void put(String key, String value);
  String getString(String key, String defaultValue);
  IPreferenceStore getStore();
}
