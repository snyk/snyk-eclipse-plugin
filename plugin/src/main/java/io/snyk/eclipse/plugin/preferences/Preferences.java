package io.snyk.eclipse.plugin.preferences;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getBinaryDirectory;

import java.io.File;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.EnvironmentConstants;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsRuntimeEnvironment;

public class Preferences {
	private static final String FALSE = "false";
	private static final String TRUE = "true";
	private static Preferences instance;
	private static LsRuntimeEnvironment LS_RUNTIME_ENV = new LsRuntimeEnvironment();
	public static final String AUTH_TOKEN_KEY = "authtoken";
	public static final String TRUSTED_FOLDERS = "trustedFolders";
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

	private static final Set<String> encryptedPreferenceKeys = Set.of(AUTH_TOKEN_KEY);
	private final IEclipsePreferences insecurePreferences;
	private final IPreferenceStore insecureStore;
	private ISecurePreferences securePreferences;
	private IPreferenceStore secureStore;
	private boolean secureStorageReady;
	private Map<String, String> prefSaveMap = new ConcurrentHashMap<>();

	public static synchronized Preferences getInstance() {
		if (instance == null) {
			var insecurePreferences = InstanceScope.INSTANCE.getNode(Activator.PLUGIN_ID);
			var securePreferences = SecurePreferencesFactory.getDefault().node(Activator.PLUGIN_ID);
			setCurrentPreferences(new Preferences(insecurePreferences, securePreferences));
		}
		return instance;
	}

	public static synchronized Preferences getTestInstance(IEclipsePreferences insecure, ISecurePreferences secure) { //NOPMD
		Preferences preferences = new Preferences(insecure, secure);
		setCurrentPreferences(preferences);
		return preferences;
	}

	Preferences(IEclipsePreferences insecure, ISecurePreferences secure) {
		this.insecurePreferences = insecure;
		this.securePreferences = secure;

		// create an insecure store backed by the insecure preferences
		this.insecureStore = new ScopedPreferenceStore(InstanceScope.INSTANCE, Activator.PLUGIN_ID);
		this.secureStore = new SecurePreferenceStore(this.securePreferences);

		CompletableFuture.runAsync(() -> {
			waitForSecureStorage();
		});

		if (getPref(ACTIVATE_SNYK_CODE_SECURITY) == null) {
			store(ACTIVATE_SNYK_CODE_SECURITY, FALSE);
		}
		if (getPref(ACTIVATE_SNYK_CODE_QUALITY) == null) {
			store(ACTIVATE_SNYK_CODE_QUALITY, FALSE);
		}
		if (getPref(ACTIVATE_SNYK_OPEN_SOURCE) == null) {
			store(ACTIVATE_SNYK_OPEN_SOURCE, TRUE);
		}
		if (getPref(ACTIVATE_SNYK_IAC) == null) {
			store(ACTIVATE_SNYK_IAC, TRUE);
		}
		if (getPref(FILTER_SHOW_CRITICAL) == null) {
			store(FILTER_SHOW_CRITICAL, TRUE);
		}
		if (getPref(FILTER_SHOW_HIGH) == null) {
			store(FILTER_SHOW_HIGH, TRUE);
		}
		if (getPref(FILTER_SHOW_MEDIUM) == null) {
			store(FILTER_SHOW_MEDIUM, TRUE);
		}
		if (getPref(FILTER_SHOW_LOW) == null) {
			store(FILTER_SHOW_LOW, TRUE);
		}
		if (getPref(ENABLE_DELTA) == null) {
			store(ENABLE_DELTA, FALSE);
		}
		if (getPref(FILTER_IGNORES_SHOW_OPEN_ISSUES) == null) {
			store(FILTER_IGNORES_SHOW_OPEN_ISSUES, TRUE);
		}
		if (getPref(FILTER_IGNORES_SHOW_IGNORED_ISSUES) == null) {
			store(FILTER_IGNORES_SHOW_IGNORED_ISSUES, FALSE);
		}
		if (getPref(FILTER_SHOW_ONLY_FIXABLE) == null) {
			store(FILTER_SHOW_ONLY_FIXABLE, FALSE);
		}

		if (getPref(SEND_ERROR_REPORTS) == null) {
			store(SEND_ERROR_REPORTS, TRUE);
		}
		if (getPref(ENABLE_TELEMETRY) == null) {
			store(ENABLE_TELEMETRY, TRUE);
		}
		if (getPref(MANAGE_BINARIES_AUTOMATICALLY) == null) {
			store(MANAGE_BINARIES_AUTOMATICALLY, TRUE);
		}
		if (getPref(MANAGE_BINARIES_AUTOMATICALLY) == null) {
			store(MANAGE_BINARIES_AUTOMATICALLY, TRUE);
		}
		if (getPref(LSP_VERSION) == null) {
			store(LSP_VERSION, "1");
		}
		if (getPref(IS_GLOBAL_IGNORES_FEATURE_ENABLED) == null) {
			store(IS_GLOBAL_IGNORES_FEATURE_ENABLED, FALSE);
		}

		String token = SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_TOKEN, "");
		if (getPref(AUTH_TOKEN_KEY) == null && !"".equals(token)) {
			store(AUTH_TOKEN_KEY, token);
		}

		String endpoint = SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_API, "");
		if (getPref(ENDPOINT_KEY) == null) {
			if (endpoint == null || endpoint.isBlank()) {
				endpoint = DEFAULT_ENDPOINT;
			}
			store(ENDPOINT_KEY, endpoint);
		}

		String org = SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_ORG, "");
		if (getPref(ORGANIZATION_KEY) == null && !"".equals(org)) {
			store(ORGANIZATION_KEY, org);
		}

		String defaultCLIPath = getDefaultCliPath();
		final var cliPath = getPref(CLI_PATH);
		if ((cliPath == null || CLI_PATH.equals(cliPath)) && !"".equals(defaultCLIPath)) {
			store(CLI_PATH, defaultCLIPath);
		}

		if (getPref(CLI_BASE_URL) == null || getPref(CLI_BASE_URL).isBlank()) {
			store(CLI_BASE_URL, "https://downloads.snyk.io");
		}

		if (getPref(SCANNING_MODE_AUTOMATIC) == null) {
			insecure.put(SCANNING_MODE_AUTOMATIC, TRUE);
		}

		if (getPref(USE_TOKEN_AUTH) == null) {
			insecure.put(USE_TOKEN_AUTH, FALSE);
		}

		if (getPref(ANALYTICS_PLUGIN_INSTALLED_SENT) == null) {
			insecure.put(ANALYTICS_PLUGIN_INSTALLED_SENT, FALSE);
		}

		String deviceId = getPref(DEVICE_ID);
		if (deviceId == null || deviceId.isBlank()) {
			insecure.put(DEVICE_ID, UUID.randomUUID().toString());
		}

		String releaseChannel = getPref(RELEASE_CHANNEL);
		if (releaseChannel == null || releaseChannel.isBlank()) {
			insecure.put(RELEASE_CHANNEL, "stable");
		}
	}

	public final void waitForSecureStorage() {
		while (true) {
			try {
				this.securePreferences.put("canEncrypt", TRUE, true);
				for (var entry : this.prefSaveMap.entrySet()) {
					this.securePreferences.put(entry.getKey(), entry.getValue(), true);
				}
				this.prefSaveMap.clear();
				this.secureStorageReady = true;
				break;
			} catch (NullPointerException | StorageException e) {
				try {
					Thread.sleep(2000);
				} catch (InterruptedException ie) {
					Thread.currentThread().interrupt();
				}
			}
		}
	}

	private final String getDefaultCliPath() {
		File binary = new File(getBinaryDirectory(), LS_RUNTIME_ENV.getDownloadBinaryName());
		final var dir = binary.getParentFile();
		if (!dir.exists()) {
			dir.mkdirs();
		}
		return binary.getAbsolutePath();
	}

	public final String getPref(String key) {
		return getPref(key, null);
	}

	public final String getPref(String key, String defaultValue) {
		if (encryptedPreferenceKeys.contains(key)) {
			if (this.isSecureStorageReady()) {
				try {
					return this.securePreferences.get(key, defaultValue);
				} catch (StorageException e) {
					SnykLogger.logError(e);
					return defaultValue;
				}
			} else {
				if (this.prefSaveMap.containsKey(key)) {
					return this.prefSaveMap.get(key);
				} else {
					return defaultValue;
				}
			}
		} else {
			return this.insecureStore.getString(key);
		}
	}

	public final String getAuthToken() {
		return getPref(AUTH_TOKEN_KEY, "");
	}

	public final String getEndpoint() {
		return getPref(ENDPOINT_KEY, DEFAULT_ENDPOINT);
	}

	public final String getLspVersion() {
		return getPref(LSP_VERSION);
	}

	public final Optional<String> getPath() {
		String path = getPref(PATH_KEY);
		if (path == null || path.isEmpty()) {
			return Optional.empty();
		}
		return Optional.of(path);
	}

	public final String getCliPath() {
		return getPref(CLI_PATH, getDefaultCliPath());
	}

	public final boolean isInsecure() {
		return insecurePreferences.getBoolean(INSECURE_KEY, false);
	}

	public final void setIsInsecure(boolean isInsecure) {
		insecurePreferences.put(INSECURE_KEY, Boolean.toString(isInsecure));
	}

	public final  boolean isManagedBinaries() {
		return insecurePreferences.getBoolean(MANAGE_BINARIES_AUTOMATICALLY, true);
	}

	public final void store(String key, String value) {
		if (key == null || value == null) return;
		if (encryptedPreferenceKeys.contains(key)) {
			if (isSecureStorageReady()) {
				try {
					securePreferences.put(key, value, true);
				} catch (StorageException e) {
					throw new RuntimeException(e);
				}
			} else {
				this.prefSaveMap.put(key, value);
			}
		} else {
			insecureStore.putValue(key, value);
		}
	}

	public final boolean getBooleanPref(String key) {
		return insecurePreferences.getBoolean(key, false);
	}

	public final boolean getBooleanPref(String key, boolean defaultValue) {
		return insecurePreferences.getBoolean(key, defaultValue);
	}

	public final String getReleaseChannel() {
		return getPref(RELEASE_CHANNEL, "stable");
	}

	public void setTest(boolean b) {
		insecurePreferences.put("isTesting", Boolean.toString(b));
	}

	public boolean isTest() {
		return getBooleanPref("isTesting", false);
	}

	public static boolean isDeltaEnabled() {
		return getInstance().getBooleanPref(Preferences.ENABLE_DELTA);
	}

	public static void setCurrentPreferences(Preferences prefs) {
		instance = prefs;
	}

	public final IPreferenceStore getInsecureStore() {
		return this.insecureStore;
	}

	public final IPreferenceStore getSecureStore() {
		return this.secureStore;
	}

	public final boolean isSecureStorageReady() {
		return secureStorageReady;
	}

	public void setSecureStorageReady(boolean b) {
		this.secureStorageReady = b;
	}
}
