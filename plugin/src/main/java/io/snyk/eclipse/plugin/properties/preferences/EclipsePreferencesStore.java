package io.snyk.eclipse.plugin.properties.preferences;

import io.snyk.eclipse.plugin.properties.SnykSecurePreferenceStore;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.preference.IPreferenceStore;


public class EclipsePreferencesStore implements PreferencesStore{

  private static final String QUALIFIER = "io.snyk.eclipse.plugin";

  private final ISecurePreferences node = SecurePreferencesFactory.getDefault().node(QUALIFIER);
  private final IPreferenceStore store = new SnykSecurePreferenceStore(node, QUALIFIER);
  @Override
  public boolean getBoolean(String key, boolean defaultValue) {
    try {
      return node.getBoolean(key, defaultValue);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public void put(String key, String value) {
    try {
      node.put(key, value, true);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public String getString(String key, String defaultValue) {
    try {
      return node.get(key, defaultValue);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public IPreferenceStore getStore() {
    return store;
  }
}
