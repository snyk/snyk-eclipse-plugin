package io.snyk.eclipse.plugin.preferences;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getBinaryDirectory;

import java.io.File;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.jface.preference.IPreferenceStore;

import io.snyk.eclipse.plugin.EnvironmentConstants;
import io.snyk.languageserver.LsRuntimeEnvironment;

public class Preferences {
	private static Preferences CURRENT_PREFERENCES;
	private static LsRuntimeEnvironment LS_RUNTIME_ENV = new LsRuntimeEnvironment();

	public static synchronized Preferences getInstance() {
		if (CURRENT_PREFERENCES == null) {
			setCurrentPreferences(new Preferences(new SecurePreferenceStore()));
		}
		return CURRENT_PREFERENCES;
	}

	public static synchronized Preferences getInstance(PreferenceStore store) {
		Preferences preferences = new Preferences(store);
		setCurrentPreferences(preferences);
		return preferences;
	}

	public static final String TRUSTED_FOLDERS = "trustedFolders";
	public static final String AUTH_TOKEN_KEY = "authtoken";
	public static final String PATH_KEY = "path";
	public static final String ENDPOINT_KEY = "endpoint";
	public static final String INSECURE_KEY = "insecure";
	public static final String CLI_PATH = "cli-path";
	public static final String CLI_BASE_URL = "cli-base-url";
	public static final String ACTIVATE_SNYK_CODE_SECURITY = "ACTIVATE_SNYK_CODE_SECURITY";
	public static final String ACTIVATE_SNYK_CODE_QUALITY = "ACTIVATE_SNYK_CODE_QUALITY";
	public static final String ACTIVATE_SNYK_OPEN_SOURCE = "ACTIVATE_SNYK_OPEN_SOURCE";
	public static final String ACTIVATE_SNYK_IAC = "ACTIVATE_SNYK_IAC";
	public static final String ADDITIONAL_PARAMETERS = "ADDITIONAL_PARAMETERS";
	public static final String ADDITIONAL_ENVIRONMENT = "ADDITIONAL_ENVIRONMENT";
	public static final String SEND_ERROR_REPORTS = "SEND_ERROR_REPORTS";
	public static final String LSP_VERSION = "LSP_VERSION";
	public static final String USE_TOKEN_AUTH = "useTokenAuth";
	public static final String ANALYTICS_PLUGIN_INSTALLED_SENT = "analyticsPluginInstalledSent";
	public static final String ENABLE_DELTA = "ENABLE_DELTA";
	
	
	// all filter preferences are positive: SHOW = true, HIDE = false
	public static final String FILTER_SHOW_CRITICAL = "FILTER_SHOW_CRITICAL";
	public static final String FILTER_SHOW_HIGH = "FILTER_SHOW_HIGH";
	public static final String FILTER_SHOW_MEDIUM = "FILTER_SHOW_MEDIUM";
	public static final String FILTER_SHOW_LOW = "FILTER_SHOW_LOW";
	
	public static final String FILTER_IGNORES_SHOW_OPEN_ISSUES = "FILTER_IGNORES_SHOW_OPEN_ISSUES";
	public static final String FILTER_IGNORES_SHOW_IGNORED_ISSUES = "FILTER_IGNORES_SHOW_IGNORED_ISSUES";
	public static final String FILTER_SHOW_ONLY_FIXABLE = "FILTER_SHOW_FIXABLE_AND_UNFIXABLE_ISSUES";

	// Feature flags
	public static final String IS_GLOBAL_IGNORES_FEATURE_ENABLED = "IS_GLOBAL_IGNORES_FEATURE_ENABLED";

	// This is a bit confusing - CLI takes DISABLE as env variable, but we ask for
	// ENABLE, so we need to revert it
	// when populating the environment
	public static final String ENABLE_TELEMETRY = EnvironmentConstants.ENV_DISABLE_ANALYTICS;
	public static final String MANAGE_BINARIES_AUTOMATICALLY = "SNYK_CFG_MANAGE_BINARIES_AUTOMATICALLY";
	public static final String ORGANIZATION_KEY = EnvironmentConstants.ENV_SNYK_ORG;
	public static final String SCANNING_MODE_AUTOMATIC = "scanningMode";
	public static final String DEFAULT_ENDPOINT = "https://api.snyk.io";
	public static final String DEVICE_ID = "deviceId";
	public static final String RELEASE_CHANNEL = "releaseChannel";
	

	private final PreferenceStore store;

	Preferences(PreferenceStore store) {
		this.store = store;
		if (getPref(ACTIVATE_SNYK_CODE_SECURITY) == null) {
			store(ACTIVATE_SNYK_CODE_SECURITY, "false");
		}
		if (getPref(ACTIVATE_SNYK_CODE_QUALITY) == null) {
			store(ACTIVATE_SNYK_CODE_QUALITY, "false");
		}
		if (getPref(ACTIVATE_SNYK_OPEN_SOURCE) == null) {
			store(ACTIVATE_SNYK_OPEN_SOURCE, "true");
		}
		if (getPref(ACTIVATE_SNYK_IAC) == null) {
			store(ACTIVATE_SNYK_IAC, "true");
		}
		if (getPref(FILTER_SHOW_CRITICAL) == null) {
			store(FILTER_SHOW_CRITICAL, "true");
		}
		if (getPref(FILTER_SHOW_HIGH) == null) {
			store(FILTER_SHOW_HIGH, "true");
		}
		if (getPref(FILTER_SHOW_MEDIUM) == null) {
			store(FILTER_SHOW_MEDIUM, "true");
		}
		if (getPref(FILTER_SHOW_LOW) == null) {
			store(FILTER_SHOW_LOW, "true");
		}
		if (getPref(ENABLE_DELTA) == null) {
			store(ENABLE_DELTA, "false");
		}
		if (getPref(FILTER_IGNORES_SHOW_OPEN_ISSUES) == null) {
			store(FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		}
		if (getPref(FILTER_IGNORES_SHOW_IGNORED_ISSUES) == null) {
			store(FILTER_IGNORES_SHOW_IGNORED_ISSUES, "false");
		}
		if (getPref(FILTER_SHOW_ONLY_FIXABLE) == null) {
			store(FILTER_SHOW_ONLY_FIXABLE, "true");
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
		if (getPref(IS_GLOBAL_IGNORES_FEATURE_ENABLED) == null) {
			store(IS_GLOBAL_IGNORES_FEATURE_ENABLED, "false");
		}

		String token = SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_TOKEN, "");
		if (getPref(AUTH_TOKEN_KEY) == null && !"".equals(token)) {
			store(AUTH_TOKEN_KEY, token);
		}

		String endpoint = SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_API, "");
		if (getPref(ENDPOINT_KEY) == null) {
			if ("".equals(endpoint)) {
				endpoint = DEFAULT_ENDPOINT;
			}
			store(ENDPOINT_KEY, endpoint);
		}

		String org = SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_ORG, "");
		if (getPref(ORGANIZATION_KEY) == null && !"".equals(org)) {
			store(ORGANIZATION_KEY, org);
		}

		String cliPath = getDefaultCliPath();
		if (getPref(CLI_PATH) == null && !"".equals(cliPath)) {
			store(CLI_PATH, cliPath);
		}

		if (getPref(CLI_BASE_URL) == null || getPref(CLI_BASE_URL).isBlank()) {
			store(CLI_BASE_URL, "https://downloads.snyk.io");
		}

		if (getPref(SCANNING_MODE_AUTOMATIC) == null) {
			store.put(SCANNING_MODE_AUTOMATIC, "true");
		}

		if (getPref(USE_TOKEN_AUTH) == null) {
			store.put(USE_TOKEN_AUTH, "false");
		}

		if (getPref(ANALYTICS_PLUGIN_INSTALLED_SENT) == null) {
			store.put(ANALYTICS_PLUGIN_INSTALLED_SENT, "false");
		}

		String deviceId = getPref(DEVICE_ID);
		if (deviceId == null || deviceId.isBlank()) {
			store.put(DEVICE_ID, UUID.randomUUID().toString());
		}

		String releaseChannel = getPref(RELEASE_CHANNEL);
		if (releaseChannel == null || releaseChannel.isBlank()) {
			store.put(RELEASE_CHANNEL, "stable");
		}
	}

	private String getDefaultCliPath() {
		File binary = new File(getBinaryDirectory(), LS_RUNTIME_ENV.getDownloadBinaryName());
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
		return getPref(CLI_PATH, getDefaultCliPath());
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

	public boolean getBooleanPref(String key, boolean defaultValue) {
		return store.getBoolean(key, defaultValue);
	}

	public String getReleaseChannel() {
		return getPref(RELEASE_CHANNEL, "stable");
	}

	public void setTest(boolean b) {
		store.put("isTesting", Boolean.toString(b));
	}

	public boolean isTest() {
		return getBooleanPref("isTesting");
	}

	public static boolean isDeltaEnabled() {
		return Preferences.getInstance().getBooleanPref(Preferences.ENABLE_DELTA);
	}

	public static void setCurrentPreferences(Preferences prefs) {
		CURRENT_PREFERENCES = prefs;
	}

}
