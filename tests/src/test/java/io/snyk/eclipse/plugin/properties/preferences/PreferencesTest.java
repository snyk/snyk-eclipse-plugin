package io.snyk.eclipse.plugin.properties.preferences;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.SystemUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import io.snyk.eclipse.plugin.EnvironmentConstants;
import io.snyk.languageserver.LsRuntimeEnvironment;

class PreferencesTest {

	private Preferences preferences;

	@BeforeEach
	void setUp() {
		PreferencesUtils.setPreferences(null);

		preferences = Mockito.mock(Preferences.class);

		Mockito.doCallRealMethod().when(preferences).anyPreferenceFalse(Mockito.anyList());
		Mockito.doCallRealMethod().when(preferences).anyPreferenceTrue(Mockito.anyList());

	}

	@Test
	void test_DefaultPreferences() {
		Preferences preferences = Preferences.getInstance(new InMemoryPreferenceStore());
		LsRuntimeEnvironment lsRuntimeEnv = new LsRuntimeEnvironment();

		assertEquals("false", preferences.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY));
		assertEquals("false", preferences.getPref(Preferences.ACTIVATE_SNYK_CODE_QUALITY));
		assertEquals("true", preferences.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
		assertEquals("true", preferences.getPref(Preferences.ACTIVATE_SNYK_IAC));
		assertEquals("false", preferences.getPref(Preferences.FILTER_CRITICAL));
		assertEquals("false", preferences.getPref(Preferences.FILTER_HIGH));
		assertEquals("false", preferences.getPref(Preferences.FILTER_MEDIUM));
		assertEquals("false", preferences.getPref(Preferences.FILTER_LOW));
		assertEquals("false", preferences.getPref(Preferences.FILTER_DELTA_NEW_ISSUES));
		assertEquals("true", preferences.getPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES));
		assertEquals("true", preferences.getPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES));
		assertEquals("false", preferences.getPref(Preferences.FILTER_FIXABLE_ISSUES));
		assertEquals("false", preferences.getPref(Preferences.FILTER_OSS_FIXABLE_ISSUES));
		assertEquals("true", preferences.getPref(Preferences.SEND_ERROR_REPORTS));
		assertEquals("true", preferences.getPref(Preferences.ENABLE_TELEMETRY));
		assertEquals("true", preferences.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY));
		assertEquals("1", preferences.getPref(Preferences.LSP_VERSION));
		assertEquals("false", preferences.getPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED));
		assertEquals("https://api.snyk.io", preferences.getPref(Preferences.ENDPOINT_KEY));
		assertEquals("https://downloads.snyk.io", preferences.getPref(Preferences.CLI_BASE_URL));
		assertEquals("true", preferences.getPref(Preferences.SCANNING_MODE_AUTOMATIC));
		assertEquals("false", preferences.getPref(Preferences.USE_TOKEN_AUTH));
		assertEquals("false", preferences.getPref(Preferences.ANALYTICS_PLUGIN_INSTALLED_SENT));
		assertEquals("stable", preferences.getPref(Preferences.RELEASE_CHANNEL));
		assertTrue(preferences.getCliPath().endsWith(lsRuntimeEnv.getDownloadBinaryName()));
	}

	@Test
	void test_ExistingTokenInEnvironment_IsStoredInPreferences() {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_TOKEN, ""))
					.thenReturn("token");

			Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

			assertEquals(prefs.getAuthToken(), "token");
		}
	}

	@Test
	void test_ExistingEndpointInEnvironment_IsStoredInPreferences() {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_API, ""))
					.thenReturn("https://custom.endpoint.io");

			Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

			assertEquals(prefs.getEndpoint(), "https://custom.endpoint.io");
		}
	}

	@Test
	void test_ExistingOrgInEnvironment_IsStoredInPreferences() {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_ORG, ""))
					.thenReturn("myOrg");

			Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

			assertEquals(prefs.getPref(Preferences.ORGANIZATION_KEY), "myOrg");
		}
	}

	@Test
	void test_GetBoolean_returnsBooleanProperty() {
		Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

		assertFalse(prefs.getBooleanPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY));
		assertTrue(prefs.getBooleanPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
	}

	@Test
	void test_GetBoolean_returnsFalseForNonBooleanProperty() {
		Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

		assertFalse(prefs.getBooleanPref(Preferences.CLI_PATH));
	}

	@Test
	public void testGetPath() {
		Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());
		prefs.store(Preferences.PATH_KEY, "/test/path");
		assertEquals(Optional.of("/test/path"), prefs.getPath());
	}

	@Test
	public void testGetPathEmpty() {
		Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());
		assertEquals(Optional.empty(), prefs.getPath());
	}

	@Test
	public void testIsInsecure() {
		Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());
		prefs.setIsInsecure(true);
		assertTrue(prefs.isInsecure());
	}

	@Test
	public void testIsManagedBinaries() {
		Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());
		assertTrue(prefs.isManagedBinaries());
		prefs.store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "false");
		assertFalse(prefs.isManagedBinaries());
	}

	@Test
	public void testGetReleaseChannel() {
		Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());
		assertEquals("stable", prefs.getReleaseChannel());
		prefs.store(Preferences.RELEASE_CHANNEL, "beta");
		assertEquals("beta", prefs.getReleaseChannel());
	}

	@Test
	public void testSetAndIsTest() {
		Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());
		assertFalse(prefs.isTest());
		prefs.setTest(true);
		assertTrue(prefs.isTest());
	}

	@Test
	public void testGetLspVersion() {
		Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());
		assertEquals("1", prefs.getLspVersion());
		prefs.store(Preferences.LSP_VERSION, "2");
		assertEquals("2", prefs.getLspVersion());
	}

	@Test
	void anyPreferenceFalse_allTrue_returnsFalse() {
		List<String> keys = Arrays.asList("key1", "key2", "key3");
		when(preferences.getBooleanPref("key1")).thenReturn(true);
		when(preferences.getBooleanPref("key2")).thenReturn(true);
		when(preferences.getBooleanPref("key3")).thenReturn(true);

		assertFalse(preferences.anyPreferenceFalse(keys));
	}

	@Test
	void anyPreferenceFalse_oneFalse_returnsTrue() {
		List<String> keys = Arrays.asList("key1", "key2", "key3");
		when(preferences.getBooleanPref("key1")).thenReturn(true);
		when(preferences.getBooleanPref("key2")).thenReturn(false);
		when(preferences.getBooleanPref("key3")).thenReturn(true);

		assertTrue(preferences.anyPreferenceFalse(keys));
	}

	@Test
	void anyPreferenceFalse_allFalse_returnsTrue() {
		List<String> keys = Arrays.asList("key1", "key2", "key3");
		when(preferences.getBooleanPref("key1")).thenReturn(false);
		when(preferences.getBooleanPref("key2")).thenReturn(false);
		when(preferences.getBooleanPref("key3")).thenReturn(false);

		assertTrue(preferences.anyPreferenceFalse(keys));
	}

	@Test
	void anyPreferenceFalse_emptyList_returnsFalse() {
		List<String> keys = Arrays.asList();

		assertFalse(preferences.anyPreferenceFalse(keys));
	}

	@Test
	void anyPreferenceTrue_allFalse_returnsFalse() {
		List<String> keys = Arrays.asList("key1", "key2", "key3");
		when(preferences.getBooleanPref("key1")).thenReturn(false);
		when(preferences.getBooleanPref("key2")).thenReturn(false);
		when(preferences.getBooleanPref("key3")).thenReturn(false);

		assertFalse(preferences.anyPreferenceTrue(keys));
	}

	@Test
	void anyPreferenceTrue_oneTrue_returnsTrue() {
		List<String> keys = Arrays.asList("key1", "key2", "key3");
		when(preferences.getBooleanPref("key1")).thenReturn(false);
		when(preferences.getBooleanPref("key2")).thenReturn(true);
		when(preferences.getBooleanPref("key3")).thenReturn(false);

		assertTrue(preferences.anyPreferenceTrue(keys));
	}

	@Test
	void anyPreferenceTrue_allTrue_returnsTrue() {
		List<String> keys = Arrays.asList("key1", "key2", "key3");
		when(preferences.getBooleanPref("key1")).thenReturn(true);
		when(preferences.getBooleanPref("key2")).thenReturn(true);
		when(preferences.getBooleanPref("key3")).thenReturn(true);

		assertTrue(preferences.anyPreferenceTrue(keys));
	}

	@Test
	void anyPreferenceTrue_emptyList_returnsFalse() {
		List<String> keys = Arrays.asList();

		assertFalse(preferences.anyPreferenceTrue(keys));
	}

}
