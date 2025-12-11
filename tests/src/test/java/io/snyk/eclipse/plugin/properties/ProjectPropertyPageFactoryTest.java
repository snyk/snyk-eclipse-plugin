package io.snyk.eclipse.plugin.properties;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.eclipse.core.runtime.CoreException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.preferences.InMemorySecurePreferenceStore;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;

class ProjectPropertyPageFactoryTest {

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
	void create_returnsProjectPropertyPage_whenNewConfigDialogEnabled() throws CoreException {
		prefs.store(Preferences.USE_LS_HTML_CONFIG_DIALOG, "true");

		ProjectPropertyPageFactory factory = new ProjectPropertyPageFactory();
		Object result = factory.create();

		assertInstanceOf(ProjectPropertyPage.class, result);
	}

	@Test
	void create_returnsNativeProjectPropertyPage_whenNewConfigDialogDisabled() throws CoreException {
		prefs.store(Preferences.USE_LS_HTML_CONFIG_DIALOG, "false");

		ProjectPropertyPageFactory factory = new ProjectPropertyPageFactory();
		Object result = factory.create();

		assertInstanceOf(NativeProjectPropertyPage.class, result);
	}
}
