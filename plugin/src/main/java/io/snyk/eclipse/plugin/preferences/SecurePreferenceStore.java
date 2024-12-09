package io.snyk.eclipse.plugin.preferences;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

public class SecurePreferenceStore extends ScopedPreferenceStore implements PreferenceStore {
  public static final String QUALIFIER = "io.snyk.eclipse.plugin";

  private final ISecurePreferences node;

  public SecurePreferenceStore() {
    super(InstanceScope.INSTANCE, QUALIFIER);
    ISecurePreferences secureStorage = SecurePreferencesFactory.getDefault();
    if (secureStorage == null) {
    	PlatformUI.getWorkbench().getDisplay().asyncExec(() -> {
    		Display display = PlatformUI.getWorkbench().getDisplay();
			Shell activeShell = display.getActiveShell();
    		String message = "Eclipse was unable to create or access the Secure Storage mechanism. "
    				+ "Please check your Secure Storage in Eclipse preferences under "
    				+ "General -> Security -> Secure Storage. "
    				+ "The Snyk plugin will not be able to work reliably and save preferences "
    				+ "or the authentication token until Secure Storage can be used.";
			String title = "Error accessing Eclipse Secure Storage (Snyk)";
			MessageDialog.openError(activeShell, title, message);
    	});
    }
	node = secureStorage.node(QUALIFIER);
  }

  @Override
  public double getDouble(String name) {
    try {
      return node.getDouble(name, DOUBLE_DEFAULT_DEFAULT);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public float getFloat(String name) {
    try {
      return node.getFloat(name, FLOAT_DEFAULT_DEFAULT);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public int getInt(String name) {
    try {
      return node.getInt(name, INT_DEFAULT_DEFAULT);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public long getLong(String name) {
    try {
      return node.getLong(name, LONG_DEFAULT_DEFAULT);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
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
    return this;
  }
}
