package io.snyk.eclipse.plugin.preferences;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

public class SecurePreferenceStore extends ScopedPreferenceStore implements IPreferenceStore {
  public static final String QUALIFIER = "io.snyk.eclipse.plugin";
  private ISecurePreferences node;

  public SecurePreferenceStore(ISecurePreferences node) {
    super(InstanceScope.INSTANCE, QUALIFIER);
    this.node = node;
  }

   @Override
  public String getString(String name) {
    try {
      return node.get(name, STRING_DEFAULT_DEFAULT);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public void putValue(String name, String value) {
    try {
      node.put(name, value, true);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public void setValue(String name, double value) {
    try {
      node.putDouble(name, value, true);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public void setValue(String name, float value) {
    try {
      node.putFloat(name, value, true);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public void setValue(String name, int value) {
    try {
      node.putInt(name, value, true);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public void setValue(String name, long value) {
    try {
      node.putLong(name, value, true);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public void setValue(String name, String value) {
    try {
      node.put(name, value, true);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public void setValue(String name, boolean value) {
    try {
      node.putBoolean(name, value, true);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public boolean contains(String name) {
    return node.nodeExists(name);
  }

  @Override
  public boolean getBoolean(String name) {
    try {
      return node.getBoolean(name, BOOLEAN_DEFAULT_DEFAULT);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }
}
