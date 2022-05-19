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
  public static final String ACTIVATE_SNYK_CODE = "ACTIVATE_SNYK_CODE";
  public static final String ACTIVATE_SNYK_OPEN_SOURCE = "ACTIVATE_SNYK_OPEN_SOURCE";
  public static final String ACTIVATE_SNYK_IAC = "ACTIVATE_SNYK_IAC";
  public static final String ADDITIONAL_PARAMETERS = "ADDITIONAL_PARAMETERS";
  public static final String ADDITIONAL_ENVIRONMENT = "ADDITIONAL_ENVIRONMENT";
  public static final String SEND_ERROR_REPORTS = "SEND_ERROR_REPORTS";

  // This is a bit confusing - CLI takes DISABLE as env variable, but we ask for ENABLE, so we need to revert it
  // when populating the environment
  public static final String ENABLE_TELEMETRY = "SNYK_CFG_DISABLE_ANALYTICS";
  public static final String ORGANIZATION_KEY = "SNYK_ORGANIZATION";


  private final ISecurePreferences node = SecurePreferencesFactory.getDefault().node(QUALIFIER);
  private final IPreferenceStore store = new SnykSecurePreferenceStore(node, QUALIFIER);

  public Preferences() {
    if (getPref(ACTIVATE_SNYK_CODE) == null) {
      store(ACTIVATE_SNYK_CODE, "false");
    }
    if (getPref(ACTIVATE_SNYK_OPEN_SOURCE) == null) {
      store(ACTIVATE_SNYK_OPEN_SOURCE, "true");
    }
    if (getPref(ACTIVATE_SNYK_IAC) == null) {
      store(ACTIVATE_SNYK_IAC, "true");
    }
    if (getPref(SEND_ERROR_REPORTS) == null) {
      store(SEND_ERROR_REPORTS, "true");
    }
    if (getPref(ENABLE_TELEMETRY) == null) {
      store(ENABLE_TELEMETRY, "false");
    }
  }

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
