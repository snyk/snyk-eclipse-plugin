package io.snyk.eclipse.plugin.properties.preferences;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.jface.preference.IPreferenceStore;

import io.snyk.eclipse.plugin.EnvironmentConstants;

import java.io.File;
import java.util.Optional;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getBinaryDirectory;

public class Preferences {
  static Preferences CURRENT_PREFERENCES;

  public static synchronized Preferences getInstance() {
    if (CURRENT_PREFERENCES == null) {
      CURRENT_PREFERENCES = new Preferences(new SecurePreferenceStore());
    }
    return CURRENT_PREFERENCES;
  }

  public static synchronized Preferences getInstance(PreferenceStore store) {
    Preferences preferences = new Preferences(store);
    CURRENT_PREFERENCES = preferences;
    return preferences;
  }

  public static final String TRUSTED_FOLDERS = "trustedFolders";
  public static final String AUTH_TOKEN_KEY = "authtoken";
  public static final String PATH_KEY = "path";
  public static final String ENDPOINT_KEY = "endpoint";
  public static final String INSECURE_KEY = "insecure";
  public static final String LS_BINARY_KEY = "ls-binary";
  public static final String CLI_PATH = "cli-path";
  public static final String ACTIVATE_SNYK_CODE = "ACTIVATE_SNYK_CODE";
  public static final String ACTIVATE_SNYK_OPEN_SOURCE = "ACTIVATE_SNYK_OPEN_SOURCE";
  public static final String ACTIVATE_SNYK_IAC = "ACTIVATE_SNYK_IAC";
  public static final String ADDITIONAL_PARAMETERS = "ADDITIONAL_PARAMETERS";
  public static final String ADDITIONAL_ENVIRONMENT = "ADDITIONAL_ENVIRONMENT";
  public static final String SEND_ERROR_REPORTS = "SEND_ERROR_REPORTS";
  public static final String LSP_VERSION = "LSP_VERSION";

  // This is a bit confusing - CLI takes DISABLE as env variable, but we ask for ENABLE, so we need to revert it
  // when populating the environment
  public static final String ENABLE_TELEMETRY = EnvironmentConstants.ENV_DISABLE_ANALYTICS;
  public static final String MANAGE_BINARIES_AUTOMATICALLY = "SNYK_CFG_MANAGE_BINARIES_AUTOMATICALLY";
  public static final String ORGANIZATION_KEY = EnvironmentConstants.ENV_SNYK_ORG;
  public static final String AUTHENTICATION_METHOD = "AUTHENTICATION_METHOD";
  public static final String AUTH_METHOD_TOKEN = "token";
  public static final String AUTH_METHOD_OAUTH = "oauth";

  private final PreferenceStore store;

  Preferences(PreferenceStore store) {
    this.store = store;
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
      store(ENABLE_TELEMETRY, "true");
    }
    if (getPref(MANAGE_BINARIES_AUTOMATICALLY) == null) {
      store(MANAGE_BINARIES_AUTOMATICALLY, "true");
    }
    if (getPref(MANAGE_BINARIES_AUTOMATICALLY) == null) {
      store(MANAGE_BINARIES_AUTOMATICALLY, "true");
    }
    if (getPref(LSP_VERSION) == null) {
      store(LSP_VERSION, "1");
    }
    
    if (getPref(AUTHENTICATION_METHOD) == null || getPref(AUTHENTICATION_METHOD).isBlank()) {
      store(AUTHENTICATION_METHOD, AUTH_METHOD_TOKEN);
    }
    
    if (getPref(LS_BINARY_KEY) == null || getPref(LS_BINARY_KEY).equals("")) {
      store(LS_BINARY_KEY, getDefaultLsPath());
    }

    String token = SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_TOKEN, "");
    if (getPref(AUTH_TOKEN_KEY) == null && !"".equals(token)) {
      store(AUTH_TOKEN_KEY, token);
    }
    String endpoint = SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_API, "");
    if (getPref(ENDPOINT_KEY) == null && !"".equals(endpoint)) {
      store(ENDPOINT_KEY, endpoint);
    }
    String org = SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_ORG, "");
    if (getPref(ORGANIZATION_KEY) == null && !"".equals(org)) {
      store(ORGANIZATION_KEY, org);
    }
  }

  private String getBinaryName() {
    var osName = SystemUtils.OS_NAME;
    var executable = "snyk-ls";
    if (osName.toLowerCase().startsWith("win")) executable += ".exe";
    return executable;
  }

  private String getDefaultLsPath() {
    File binary = new File(getBinaryDirectory(), getBinaryName());
    return binary.getAbsolutePath();
  }

  public String getPref(String key) {
    return getPref(key, null);
  }

  public String getPref(String key, String defaultValue) {
    return store.getString(key, defaultValue);
  }

  public String getAuthToken() {
    return getPref(AUTH_TOKEN_KEY, "");
  }

  public String getEndpoint() {
    return getPref(ENDPOINT_KEY, "");
  }

  public String getLsBinary() {
    return getPref(LS_BINARY_KEY, "");
  }
  public String getLspVersion() {
    return getPref(LSP_VERSION);
  }

  public Optional<String> getPath() {
    String path = getPref(PATH_KEY);
    if (path == null || path.isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(path);
  }

  public String getCliPath() {
    return getPref(CLI_PATH, "");
  }

  public boolean isInsecure() {
    return store.getBoolean(INSECURE_KEY, false);
  }

  public void setIsInsecure(boolean isInsecure) {
	  store.put(INSECURE_KEY, Boolean.toString(isInsecure));
  }


  public boolean isManagedBinaries() {
    return store.getBoolean(MANAGE_BINARIES_AUTOMATICALLY, true);
  }

  public void store(String key, String value) {
    store.put(key, value);
  }

  public IPreferenceStore getStore() {
    return store.getStore();
  }

  public boolean getBooleanPref(String key) {
   return store.getBoolean(key, false);
  }
}

