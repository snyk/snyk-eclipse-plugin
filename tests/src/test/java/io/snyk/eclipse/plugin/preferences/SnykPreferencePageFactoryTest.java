package io.snyk.eclipse.plugin.preferences;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.eclipse.core.runtime.CoreException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;

class SnykPreferencePageFactoryTest {

	private Preferences prefs;

	@BeforeEach
	void setUp() {
		InMemoryPreferenceStore store = new InMemoryPreferenceStore();
		InMemorySecurePreferenceStore secureStore = new InMemorySecurePreferenceStore();
		prefs = Preferences.getTestInstance(store, secureStore);
		PreferencesUtils.setPreferences(prefs);
		Preferences.setEnvProvider(k -> null);
	}

	@AfterEach
	void tearDown() {
		PreferencesUtils.setPreferences(null);
		Preferences.setEnvProvider(null);
	}

	@Test
	void create_returnsHTMLSettingsPreferencePage_whenNewConfigDialogEnabled() throws CoreException {
		Preferences.setEnvProvider(k -> "SNYK_USE_HTML_SETTINGS".equals(k) ? "true" : null);

		SnykPreferencePageFactory factory = new SnykPreferencePageFactory();
		Object result = factory.create();

		assertInstanceOf(HTMLSettingsPreferencePage.class, result);
	}

	@Test
	void create_returnsPreferencesPage_whenNewConfigDialogDisabled() throws CoreException {
		Preferences.setEnvProvider(k -> "SNYK_USE_HTML_SETTINGS".equals(k) ? "false" : null);

		SnykPreferencePageFactory factory = new SnykPreferencePageFactory();
		Object result = factory.create();

		assertInstanceOf(PreferencesPage.class, result);
	}

	@Test
	void create_returnsHTMLSettingsPreferencePage_whenEnvVarUnset() throws CoreException {
		Preferences.setEnvProvider(k -> null);

		SnykPreferencePageFactory factory = new SnykPreferencePageFactory();
		Object result = factory.create();

		assertInstanceOf(HTMLSettingsPreferencePage.class, result);
	}
}
