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
	}

	@AfterEach
	void tearDown() {
		PreferencesUtils.setPreferences(null);
	}

	@Test
	void create_returnsHTMLSettingsPreferencePage_whenNewConfigDialogEnabled() throws CoreException {
		prefs.store(Preferences.USE_LS_HTML_CONFIG_DIALOG, "true");

		SnykPreferencePageFactory factory = new SnykPreferencePageFactory();
		Object result = factory.create();

		assertInstanceOf(HTMLSettingsPreferencePage.class, result);
	}

	@Test
	void create_returnsPreferencesPage_whenNewConfigDialogDisabled() throws CoreException {
		prefs.store(Preferences.USE_LS_HTML_CONFIG_DIALOG, "false");

		SnykPreferencePageFactory factory = new SnykPreferencePageFactory();
		Object result = factory.create();

		assertInstanceOf(PreferencesPage.class, result);
	}
}
