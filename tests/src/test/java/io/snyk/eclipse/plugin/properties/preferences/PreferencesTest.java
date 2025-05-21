package io.snyk.eclipse.plugin.properties.preferences;

import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_QUALITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_SECURITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_IAC;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_OPEN_SOURCE;
import static io.snyk.eclipse.plugin.preferences.Preferences.ANALYTICS_PLUGIN_INSTALLED_SENT;
import static io.snyk.eclipse.plugin.preferences.Preferences.CLI_BASE_URL;
import static io.snyk.eclipse.plugin.preferences.Preferences.ENABLE_DELTA;
import static io.snyk.eclipse.plugin.preferences.Preferences.ENABLE_TELEMETRY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ENDPOINT_KEY;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_ONLY_FIXABLE;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_CRITICAL;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_HIGH;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_LOW;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_MEDIUM;
import static io.snyk.eclipse.plugin.preferences.Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED;
import static io.snyk.eclipse.plugin.preferences.Preferences.LSP_VERSION;
import static io.snyk.eclipse.plugin.preferences.Preferences.MANAGE_BINARIES_AUTOMATICALLY;
import static io.snyk.eclipse.plugin.preferences.Preferences.RELEASE_CHANNEL;
import static io.snyk.eclipse.plugin.preferences.Preferences.SCANNING_MODE_AUTOMATIC;
import static io.snyk.eclipse.plugin.preferences.Preferences.SEND_ERROR_REPORTS;
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
import io.snyk.eclipse.plugin.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.preferences.InMemorySecurePreferenceStore;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.LsRuntimeEnvironment;

class PreferencesTest {

	@BeforeEach
	void setUp() {
		PreferencesUtils.setPreferences(null);
	}

	@Test
	void test_DefaultPreferences() {
		Preferences preferences = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		LsRuntimeEnvironment lsRuntimeEnv = new LsRuntimeEnvironment();

		assertEquals("false", preferences.getPref(ACTIVATE_SNYK_CODE_SECURITY));
		assertEquals("false", preferences.getPref(ACTIVATE_SNYK_CODE_QUALITY));
		assertEquals("true", preferences.getPref(ACTIVATE_SNYK_OPEN_SOURCE));
		assertEquals("true", preferences.getPref(ACTIVATE_SNYK_IAC));
		assertEquals("true", preferences.getPref(FILTER_SHOW_CRITICAL));
		assertEquals("true", preferences.getPref(FILTER_SHOW_HIGH));
		assertEquals("true", preferences.getPref(FILTER_SHOW_MEDIUM));
		assertEquals("true", preferences.getPref(FILTER_SHOW_LOW));
		assertEquals("false", preferences.getPref(ENABLE_DELTA));
		assertEquals("true", preferences.getPref(FILTER_IGNORES_SHOW_OPEN_ISSUES));
		assertEquals("false", preferences.getPref(FILTER_IGNORES_SHOW_IGNORED_ISSUES));
		assertEquals("false", preferences.getPref(FILTER_SHOW_ONLY_FIXABLE));
		assertEquals("true", preferences.getPref(MANAGE_BINARIES_AUTOMATICALLY));
		assertEquals("1", preferences.getPref(LSP_VERSION));
		assertEquals("false", preferences.getPref(IS_GLOBAL_IGNORES_FEATURE_ENABLED));
		assertEquals("https://api.snyk.io", preferences.getPref(ENDPOINT_KEY));
		assertEquals("https://downloads.snyk.io", preferences.getPref(CLI_BASE_URL));
		assertEquals("true", preferences.getPref(SCANNING_MODE_AUTOMATIC));
		assertEquals("false", preferences.getPref(ANALYTICS_PLUGIN_INSTALLED_SENT));
		assertEquals("stable", preferences.getPref(RELEASE_CHANNEL));
		assertTrue(preferences.getCliPath().endsWith(lsRuntimeEnv.getDownloadBinaryName()));
	}

	@Test
	void test_ExistingTokenInEnvironment_IsStoredInPreferences() {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_TOKEN, ""))
					.thenReturn("token");

			Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
			prefs.waitForSecureStorage();

			assertEquals("token", prefs.getAuthToken());
		}
	}

	@Test
	void test_ExistingEndpointInEnvironment_IsStoredInPreferences() {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_API, ""))
					.thenReturn("https://custom.endpoint.io");

			Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());

			assertEquals(prefs.getEndpoint(), "https://custom.endpoint.io");
		}
	}

	@Test
	void test_ExistingOrgInEnvironment_IsStoredInPreferences() {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_ORG, ""))
					.thenReturn("myOrg");

			Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());

			assertEquals("myOrg",prefs.getPref(Preferences.ORGANIZATION_KEY));
		}
	}

	@Test
	void test_GetBoolean_returnsBooleanProperty() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());

		assertFalse(prefs.getBooleanPref(ACTIVATE_SNYK_CODE_SECURITY));
		assertTrue(prefs.getBooleanPref(ACTIVATE_SNYK_OPEN_SOURCE));
	}

	@Test
	void test_GetBoolean_returnsFalseForNonBooleanProperty() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());

		assertFalse(prefs.getBooleanPref(Preferences.CLI_PATH));
	}

	@Test
	public void testGetPath() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		prefs.store(Preferences.PATH_KEY, "/test/path");
		assertEquals(Optional.of("/test/path"), prefs.getPath());
	}

	@Test
	public void testGetPathEmpty() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		assertEquals(Optional.empty(), prefs.getPath());
	}

	@Test
	public void testIsInsecure() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		prefs.setIsInsecure(true);
		assertTrue(prefs.isInsecure());
	}

	@Test
	public void testIsManagedBinaries() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		assertTrue(prefs.isManagedBinaries());
		prefs.store(MANAGE_BINARIES_AUTOMATICALLY, "false");
		assertFalse(prefs.isManagedBinaries());
	}

	@Test
	public void testGetReleaseChannel() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		assertEquals("stable", prefs.getReleaseChannel());
		prefs.store(RELEASE_CHANNEL, "beta");
		assertEquals("beta", prefs.getReleaseChannel());
	}

	@Test
	public void testSetAndIsTest() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		assertFalse(prefs.isTest());
		prefs.setTest(true);
		assertTrue(prefs.isTest());
	}

	@Test
	public void testGetLspVersion() {
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		assertEquals("1", prefs.getLspVersion());
		prefs.store(LSP_VERSION, "2");
		assertEquals("2", prefs.getLspVersion());
	}

}
