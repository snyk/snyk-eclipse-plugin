package io.snyk.eclipse.plugin.properties;

import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.preference.IPreferenceStore;

import java.util.Optional;

public class Preferences {
  public static final String QUALIFIER = "io.snyk.eclipse.plugin";
  public static final String AUTH_TOKEN_KEY = "authtoken";
  public static final String PATH_KEY = "path";
  public static final String ENDPOINT_KEY = "endpoint";
  public static final String INSECURE_KEY = "insecure";
  public static final String LS_BINARY_KEY = "ls-binary";


  private final ISecurePreferences node = SecurePreferencesFactory.getDefault().node(QUALIFIER);
  private final IPreferenceStore store = new SnykSecurePreferenceStore(node, QUALIFIER);

  public String getPref(String key) {
    return getPref(key, null);
  }

  public String getPref(String key, String defaultValue) {
    try {
      return node.get(key, defaultValue);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  public String getAuthToken() {
    return getPref(AUTH_TOKEN_KEY, "");
  }

  public String getEndpoint() {
    return getPref(ENDPOINT_KEY);
  }

  public String getLsBinary() {
    return getPref(LS_BINARY_KEY);
  }

  public Optional<String> getPath() {
    String path = getPref(PATH_KEY);
    if (path == null || path.isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(path);
  }

  public boolean isInsecure() {
    try {
      return node.getBoolean(INSECURE_KEY, false);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  public void store(String key, String value) {
    try {
      node.put(key, value, true);
    } catch (StorageException e) {
      throw new RuntimeException(e);
    }
  }

  public IPreferenceStore getStore() {
    return store;
  }

}
