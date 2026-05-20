package io.snyk.eclipse.plugin.properties.preferences;

import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_SECURITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_OPEN_SOURCE;
import static io.snyk.eclipse.plugin.preferences.Preferences.ANALYTICS_PLUGIN_INSTALLED_SENT;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_ONLY_FIXABLE;
import static io.snyk.eclipse.plugin.preferences.Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED;
import static io.snyk.eclipse.plugin.preferences.Preferences.LSP_VERSION;
import static io.snyk.eclipse.plugin.preferences.Preferences.MANAGE_BINARIES_AUTOMATICALLY;
import static io.snyk.eclipse.plugin.preferences.Preferences.RELEASE_CHANNEL;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Optional;

import org.apache.commons.lang3.SystemUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import io.snyk.eclipse.plugin.EnvironmentConstants;
import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.preferences.InMemorySecurePreferenceStore;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.LsRuntimeEnvironment;
import io.snyk.languageserver.LsSettingsRegistry;

class PreferencesTest {

	@BeforeEach
	void setUp() {
		PreferencesUtils.setPreferences(null);
		Preferences.setEnvProvider(k -> null);
	}

	@Test
	void test_DefaultPreferences() {
		Preferences preferences = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		LsRuntimeEnvironment lsRuntimeEnv = new LsRuntimeEnvironment();

		// Registry-backed keys: assert stored default matches registry default to catch drift.
		for (LsSettingsRegistry.Entry entry : LsSettingsRegistry.ENTRIES.values()) {
			if (entry.prefKey == null || entry.encrypted || Preferences.CLI_PATH.equals(entry.prefKey)) {
				continue;
			}
			assertEquals(entry.outboundDefault, preferences.getPref(entry.prefKey),
					"Default mismatch for registry key: " + entry.lsKey);
		}

		// Non-registry keys with their own defaults.
		assertEquals("false", preferences.getPref(FILTER_SHOW_ONLY_FIXABLE));
		assertEquals("1", preferences.getPref(LSP_VERSION));
		assertEquals("false", preferences.getPref(IS_GLOBAL_IGNORES_FEATURE_ENABLED));
		assertEquals("false", preferences.getPref(ANALYTICS_PLUGIN_INSTALLED_SENT));
		assertTrue(preferences.getCliPath().endsWith(lsRuntimeEnv.getDownloadBinaryName()));
	}

	@Test
	void test_ExistingTokenInEnvironment_IsStoredInPreferences() {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_TOKEN, ""))
					.thenReturn("token");

			Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
					new InMemorySecurePreferenceStore());
			prefs.waitForSecureStorage();

			assertEquals("token", prefs.getAuthToken());
		}
	}

	@Test
	void test_ExistingEndpointInEnvironment_IsStoredInPreferences() {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_API, ""))
					.thenReturn("https://custom.endpoint.io");

			Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
					new InMemorySecurePreferenceStore());

			assertEquals(prefs.getEndpoint(), "https://custom.endpoint.io");
		}
	}

	@Test
	void test_ExistingOrgInEnvironment_IsStoredInPreferences() {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_ORG, ""))
					.thenReturn("myOrg");

			Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
					new InMemorySecurePreferenceStore());

			assertEquals("myOrg", prefs.getPref(Preferences.ORGANIZATION_KEY));
		}
	}

	@Test
	void test_GetBoolean_returnsBooleanProperty() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());

		assertFalse(prefs.getBooleanPref(ACTIVATE_SNYK_CODE_SECURITY));
		assertTrue(prefs.getBooleanPref(ACTIVATE_SNYK_OPEN_SOURCE));
	}

	@Test
	void test_GetBoolean_returnsFalseForNonBooleanProperty() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());

		assertFalse(prefs.getBooleanPref(Preferences.CLI_PATH));
	}

	@Test
	public void testGetPath() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.store(Preferences.PATH_KEY, "/test/path");
		assertEquals(Optional.of("/test/path"), prefs.getPath());
	}

	@Test
	public void testGetPathEmpty() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		assertEquals(Optional.empty(), prefs.getPath());
	}

	@Test
	public void testIsInsecure() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.setIsInsecure(true);
		assertTrue(prefs.isInsecure());
	}

	@Test
	public void testIsManagedBinaries() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		assertTrue(prefs.isManagedBinaries());
		prefs.store(MANAGE_BINARIES_AUTOMATICALLY, "false");
		assertFalse(prefs.isManagedBinaries());
	}

	@Test
	public void testGetReleaseChannel() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		assertEquals("stable", prefs.getReleaseChannel());
		prefs.store(RELEASE_CHANNEL, "beta");
		assertEquals("beta", prefs.getReleaseChannel());
	}

	@Test
	public void testSetAndIsTest() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		assertFalse(prefs.isTest());
		prefs.setTest(true);
		assertTrue(prefs.isTest());
	}

	@Test
	public void testGetLspVersion() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		assertEquals("1", prefs.getLspVersion());
		prefs.store(LSP_VERSION, "2");
		assertEquals("2", prefs.getLspVersion());
	}

	@Test
	public void testIsNewConfigDialogEnabled_defaultIsFalse() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		PreferencesUtils.setPreferences(prefs);
		assertFalse(Preferences.isNewConfigDialogEnabled());
	}

	@Test
	public void testIsNewConfigDialogEnabled_falseWhenEnvVarSetToFalse() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		PreferencesUtils.setPreferences(prefs);
		Preferences.setEnvProvider(k -> "SNYK_USE_HTML_SETTINGS".equals(k) ? "false" : null);
		assertFalse(Preferences.isNewConfigDialogEnabled());
	}

	@Test
	public void testMarkExplicitlyChanged_addsKeyToSet() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		assertFalse(prefs.isExplicitlyChanged("endpoint"));
		prefs.markExplicitlyChanged("endpoint");
		assertTrue(prefs.isExplicitlyChanged("endpoint"));
	}

	@Test
	public void testIsExplicitlyChanged_returnsFalseForUnchangedKey() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		assertFalse(prefs.isExplicitlyChanged("organization"));
	}

	@Test
	public void testClearExplicitlyChanged_removesKeyFromSet() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.markExplicitlyChanged("endpoint");
		assertTrue(prefs.isExplicitlyChanged("endpoint"));
		prefs.clearExplicitlyChanged("endpoint");
		assertFalse(prefs.isExplicitlyChanged("endpoint"));
	}

	@Test
	public void testExplicitChanges_multipleKeys() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.markExplicitlyChanged("endpoint");
		prefs.markExplicitlyChanged("organization");
		assertTrue(prefs.isExplicitlyChanged("endpoint"));
		assertTrue(prefs.isExplicitlyChanged("organization"));
		assertFalse(prefs.isExplicitlyChanged("path"));
	}

	@Test
	public void testExplicitChanges_persistedAsCommaSeparatedString() {
		InMemoryPreferenceStore store = new InMemoryPreferenceStore();
		Preferences prefs = Preferences.getTestInstance(store, new InMemorySecurePreferenceStore());
		prefs.markExplicitlyChanged("endpoint");
		prefs.markExplicitlyChanged("organization");

		String persisted = store.get(Preferences.EXPLICIT_CHANGES_KEY, "");
		assertTrue(persisted.contains("endpoint"));
		assertTrue(persisted.contains("organization"));
	}

	@Test
	public void testExplicitChanges_restoredFromPreferenceStoreOnConstruction() {
		InMemoryPreferenceStore store = new InMemoryPreferenceStore();
		store.put(Preferences.EXPLICIT_CHANGES_KEY, "endpoint,organization");

		Preferences prefs = Preferences.getTestInstance(store, new InMemorySecurePreferenceStore());
		assertTrue(prefs.isExplicitlyChanged("endpoint"));
		assertTrue(prefs.isExplicitlyChanged("organization"));
		assertFalse(prefs.isExplicitlyChanged("path"));
	}

	@Test
	public void testStoreAndTrackChange_marksChangedWhenValueDiffers() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.store(Preferences.ENDPOINT_KEY, "https://old.endpoint.io");
		assertFalse(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));

		prefs.storeAndTrackChange(Preferences.ENDPOINT_KEY, "https://new.endpoint.io");

		assertTrue(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));
		assertEquals("https://new.endpoint.io", prefs.getPref(Preferences.ENDPOINT_KEY));
	}

	@Test
	public void testStoreAndTrackChange_doesNotMarkChangedWhenValueSame() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.store(Preferences.ENDPOINT_KEY, "https://same.endpoint.io");

		prefs.storeAndTrackChange(Preferences.ENDPOINT_KEY, "https://same.endpoint.io");

		assertFalse(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));
	}

	@Test
	public void testStoreAndTrackChange_marksChangedOnFirstWrite() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());

		prefs.storeAndTrackChange(Preferences.ORGANIZATION_KEY, "my-org");

		assertTrue(prefs.isExplicitlyChanged(Preferences.ORGANIZATION_KEY));
		assertEquals("my-org", prefs.getPref(Preferences.ORGANIZATION_KEY));
	}

	@Test
	public void testStoreAndTrackChange_doesNotFalsePositiveWhenValueMatchesRegisteredDefault() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		// ACTIVATE_SNYK_OPEN_SOURCE has a registered default of "true".
		// storeAndTrackChange should compare against the registered default, not "".
		prefs.storeAndTrackChange(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "true");

		assertFalse(prefs.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_OPEN_SOURCE),
				"Should not mark as changed when value matches registered default");
	}

	@Test
	public void testStoreAndTrackChange_doesNotFalsePositiveForBooleanDefaults() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.storeAndTrackChange(Preferences.ACTIVATE_SNYK_IAC, "true");
		prefs.storeAndTrackChange(Preferences.FILTER_SHOW_CRITICAL, "true");
		prefs.storeAndTrackChange(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "true");
		prefs.storeAndTrackChange(Preferences.SCANNING_MODE_AUTOMATIC, "true");

		assertFalse(prefs.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_IAC));
		assertFalse(prefs.isExplicitlyChanged(Preferences.FILTER_SHOW_CRITICAL));
		assertFalse(prefs.isExplicitlyChanged(Preferences.MANAGE_BINARIES_AUTOMATICALLY));
		assertFalse(prefs.isExplicitlyChanged(Preferences.SCANNING_MODE_AUTOMATIC));
	}

	@Test
	public void testStoreAndTrackChange_handlesNullKeyGracefully() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.storeAndTrackChange(null, "value");
		// should not throw
	}

	@Test
	public void testStoreAndTrackChange_handlesNullValueGracefully() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.storeAndTrackChange(Preferences.ENDPOINT_KEY, null);
		assertFalse(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));
	}

	@Test
	public void testClearExplicitlyChanged_resetFlowClearsOverride() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		// User sets a value
		prefs.storeAndTrackChange(Preferences.ENDPOINT_KEY, "https://user-override.snyk.io");
		assertTrue(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));

		// User resets the value (LS form sends null)
		prefs.clearExplicitlyChanged(Preferences.ENDPOINT_KEY);
		assertFalse(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));

		// Value is still stored (clearing override doesn't erase the value)
		assertEquals("https://user-override.snyk.io", prefs.getPref(Preferences.ENDPOINT_KEY));
	}

	@Test
	public void testClearExplicitlyChanged_onlyAffectsSpecifiedKey() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(),
				new InMemorySecurePreferenceStore());
		prefs.storeAndTrackChange(Preferences.ENDPOINT_KEY, "https://custom.snyk.io");
		prefs.storeAndTrackChange(Preferences.ORGANIZATION_KEY, "my-org");
		assertTrue(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));
		assertTrue(prefs.isExplicitlyChanged(Preferences.ORGANIZATION_KEY));

		prefs.clearExplicitlyChanged(Preferences.ENDPOINT_KEY);

		assertFalse(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));
		assertTrue(prefs.isExplicitlyChanged(Preferences.ORGANIZATION_KEY));
	}

}
