package io.snyk.eclipse.plugin.properties.preferences;

import org.apache.commons.lang3.NotImplementedException;
import org.eclipse.jface.preference.IPreferenceStore;

import java.util.HashMap;
import java.util.Map;

public class InMemoryPreferenceStore implements PreferenceStore {

  private final Map<String, String> store = new HashMap<>();

  @Override
  public boolean getBoolean(String key, boolean defaultValue) {
    if (store.containsKey(key)) {
      return Boolean.parseBoolean(store.get(key));
    }
    return defaultValue;
  }

  @Override
  public void put(String key, String value) {
    store.put(key, value);
  }

  @Override
  public String getString(String key, String defaultValue) {
    if (store.containsKey(key)) {
      return store.get(key);
    }
    return defaultValue;
  }

  @Override
  public IPreferenceStore getStore() {
    throw new NotImplementedException();
  }
}
