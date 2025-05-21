package io.snyk.eclipse.plugin.preferences;

import static io.snyk.eclipse.plugin.EnvironmentConstants.ENV_SNYK_API;
import static io.snyk.eclipse.plugin.EnvironmentConstants.ENV_SNYK_ORG;
import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getBinaryDirectory;
import static org.apache.commons.lang3.SystemUtils.getEnvironmentVariable;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

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

	public static final String AUTHENTICATION_METHOD = "authenticationMethod";
	public static final String AUTH_TOKEN_KEY = "authtoken";
	private static final Set<String> encryptedPreferenceKeys = Set.of(AUTH_TOKEN_KEY);
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

	public static synchronized Preferences getTestInstance(IEclipsePreferences insecure, ISecurePreferences secure) { // NOPMD
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

		insecureStore.setDefault(ACTIVATE_SNYK_CODE_SECURITY, FALSE);
		insecureStore.setDefault(ACTIVATE_SNYK_CODE_QUALITY, FALSE);
		insecureStore.setDefault(ACTIVATE_SNYK_OPEN_SOURCE, TRUE);
		insecureStore.setDefault(ACTIVATE_SNYK_IAC, TRUE);
		insecureStore.setDefault(FILTER_SHOW_CRITICAL, TRUE);
		insecureStore.setDefault(FILTER_SHOW_HIGH, TRUE);
		insecureStore.setDefault(FILTER_SHOW_MEDIUM, TRUE);
		insecureStore.setDefault(FILTER_SHOW_LOW, TRUE);
		insecureStore.setDefault(ENABLE_DELTA, FALSE);
		insecureStore.setDefault(FILTER_IGNORES_SHOW_OPEN_ISSUES, TRUE);
		insecureStore.setDefault(FILTER_IGNORES_SHOW_IGNORED_ISSUES, FALSE);
		insecureStore.setDefault(FILTER_SHOW_ONLY_FIXABLE, FALSE);
		insecureStore.setDefault(MANAGE_BINARIES_AUTOMATICALLY, TRUE);
		insecureStore.setDefault(LSP_VERSION, "1");
		insecureStore.setDefault(IS_GLOBAL_IGNORES_FEATURE_ENABLED, FALSE);
		insecureStore.setDefault(CLI_BASE_URL, "https://downloads.snyk.io");
		insecureStore.setDefault(SCANNING_MODE_AUTOMATIC, TRUE);
		insecureStore.setDefault(AUTHENTICATION_METHOD, "authenticationMethod");
		insecureStore.setDefault(ANALYTICS_PLUGIN_INSTALLED_SENT, FALSE);
		insecureStore.setDefault(DEVICE_ID, UUID.randomUUID().toString());
		insecureStore.setDefault(RELEASE_CHANNEL, "stable");
		insecureStore.setDefault(CLI_PATH, getDefaultCliPath());
		insecureStore.setDefault(ENDPOINT_KEY, DEFAULT_ENDPOINT);
		insecureStore.setDefault(ORGANIZATION_KEY, "");

		var endpoint = getEnvironmentVariable(ENV_SNYK_API, "");
		if (endpoint != null && !endpoint.isBlank()) {
			store(ENDPOINT_KEY, endpoint);
		}

		var org = getEnvironmentVariable(ENV_SNYK_ORG, "");
		if (org != null && !org.isBlank()) {
			store(ORGANIZATION_KEY, org);
		}

		String token = getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_TOKEN, "");
		if (getPref(AUTH_TOKEN_KEY) != null && !"".equals(token)) {
			store(AUTH_TOKEN_KEY, token);
		}

		migratePreferences();
	}

	private void migratePreferences() {
		Set<String> constants = new HashSet<>(Arrays.asList(TRUSTED_FOLDERS, PATH_KEY, ENDPOINT_KEY, INSECURE_KEY,
				CLI_PATH, CLI_BASE_URL, ACTIVATE_SNYK_CODE_SECURITY, ACTIVATE_SNYK_CODE_QUALITY,
				ACTIVATE_SNYK_OPEN_SOURCE, ACTIVATE_SNYK_IAC, ADDITIONAL_PARAMETERS, ADDITIONAL_ENVIRONMENT,
				SEND_ERROR_REPORTS, LSP_VERSION, ANALYTICS_PLUGIN_INSTALLED_SENT, ENABLE_DELTA,
				FILTER_SHOW_CRITICAL, FILTER_SHOW_HIGH, FILTER_SHOW_MEDIUM, FILTER_SHOW_LOW,
				FILTER_IGNORES_SHOW_OPEN_ISSUES, FILTER_IGNORES_SHOW_IGNORED_ISSUES, FILTER_SHOW_ONLY_FIXABLE,
				IS_GLOBAL_IGNORES_FEATURE_ENABLED, ENABLE_TELEMETRY, MANAGE_BINARIES_AUTOMATICALLY, ORGANIZATION_KEY,
				SCANNING_MODE_AUTOMATIC, DEFAULT_ENDPOINT, DEVICE_ID, RELEASE_CHANNEL));

		for (String constant : constants) {
			try {
				final var defaultString = insecureStore.getDefaultString(constant);
				final var value = securePreferences.get(constant, defaultString);
				if (insecureStore.isDefault(constant) && !defaultString.equals(value)) {
					this.insecurePreferences.put(constant, value);
				}
				securePreferences.remove(constant);
			} catch (IllegalStateException | StorageException e) { // NOPMD // no handling needed in this case
			}
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
		return getPref(key, insecureStore.getDefaultString(key));
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
			return this.insecurePreferences.get(key, defaultValue);
		}
	}

	public final String getAuthToken() {
		return getPref(AUTH_TOKEN_KEY, "");
	}

	public final String getEndpoint() {
		return getPref(ENDPOINT_KEY);
	}

	public final String getLspVersion() {
		return insecurePreferences.get(LSP_VERSION, insecureStore.getDefaultString(LSP_VERSION));
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
		return insecurePreferences.getBoolean(INSECURE_KEY, insecureStore.getDefaultBoolean(INSECURE_KEY));
	}

	public final void setIsInsecure(boolean isInsecure) {
		insecurePreferences.put(INSECURE_KEY, Boolean.toString(isInsecure));
	}

	public final boolean isManagedBinaries() {
		return insecurePreferences.getBoolean(MANAGE_BINARIES_AUTOMATICALLY,
				insecureStore.getDefaultBoolean(MANAGE_BINARIES_AUTOMATICALLY));
	}

	public final void store(String key, String value) {
		if (key == null || value == null)
			return;
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
			insecurePreferences.put(key, value);
		}
	}

	public final boolean getBooleanPref(String key) {
		return insecurePreferences.getBoolean(key, insecureStore.getDefaultBoolean(key));
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

	public boolean isAuthenticated() {
		//TODO check AUTHENTICATION_METHOD, USE_TOKEN_AUTH and AUTH_TOKEN_KEY

		if (getAuthToken().isBlank()){
			return false;
		}

		return true;
	}
}
