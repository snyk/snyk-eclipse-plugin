package io.snyk.eclipse.plugin.properties;

import java.util.Optional;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import io.snyk.eclipse.plugin.Activator;

public class Preferences {
	
	public static final String QUALIFIER = "io.snyk.eclipse.plugin";
	public static final String AUTH_TOKEN_KEY = "authtoken";
	public static final String PATH_KEY = "path";
	public static final String ENDPOINT_KEY = "endpoint";
	public static final String INSECURE_KEY = "insecure";
	
	public static final ScopedPreferenceStore STORE = new ScopedPreferenceStore(InstanceScope.INSTANCE, QUALIFIER);
	
	private Preferences() {
	}
	
	public static String getPref(String key) {
		return Platform.getPreferencesService().getString(Preferences.QUALIFIER, key, null, null);
	}
	
	public static String getAuthToken() {
		return getPref(AUTH_TOKEN_KEY);
	}
	
	public static String getEndpoint() {
		return getPref(ENDPOINT_KEY);
	}
	
	public static Optional<String> getPath() {
		String path = getPref(PATH_KEY);
		if (path == null || path.isEmpty()) {
			return Optional.empty();
		}
		return Optional.of(path);
	}
	
	public static boolean isInsecure() {
		return Platform.getPreferencesService().getBoolean(Preferences.QUALIFIER, INSECURE_KEY, false, null);
	}
	
	public static void store(String key, String value) {
		IPreferenceStore prefStore = Activator.getDefault().getPreferenceStore();
		prefStore.setValue(key, value);
	}

}
