package io.snyk.eclipse.plugin.preferences;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;

class HTMLSettingsPreferencePageTest {

	private Preferences prefs;
	private HTMLSettingsPreferencePage page;

	@BeforeEach
	void setUp() {
		InMemoryPreferenceStore store = new InMemoryPreferenceStore();
		InMemorySecurePreferenceStore secureStore = new InMemorySecurePreferenceStore();
		prefs = Preferences.getTestInstance(store, secureStore);
		PreferencesUtils.setPreferences(prefs);
		page = new HTMLSettingsPreferencePage();
	}

	@AfterEach
	void tearDown() {
		PreferencesUtils.setPreferences(null);
	}

	@Test
	void parseAndSaveConfig_savesCliPath() throws Exception {
		String json = "{\"cliPath\": \"/usr/local/bin/snyk\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("/usr/local/bin/snyk", prefs.getCliPath());
	}

	@Test
	void parseAndSaveConfig_savesManageBinariesAutomatically() throws Exception {
		String json = "{\"manageBinariesAutomatically\": false}";

		invokeParseAndSaveConfig(json);

		assertFalse(prefs.isManagedBinaries());
	}

	@Test
	void parseAndSaveConfig_savesCliBaseDownloadURL() throws Exception {
		String json = "{\"cliBaseDownloadURL\": \"https://custom.downloads.snyk.io\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("https://custom.downloads.snyk.io", prefs.getPref(Preferences.CLI_BASE_URL));
	}

	@Test
	void parseAndSaveConfig_savesCliReleaseChannel() throws Exception {
		String json = "{\"cliReleaseChannel\": \"preview\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("preview", prefs.getReleaseChannel());
	}

	@Test
	void parseAndSaveConfig_savesActivateSnykOpenSource() throws Exception {
		String json = "{\"activateSnykOpenSource\": false}";

		invokeParseAndSaveConfig(json);

		assertEquals("false", prefs.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
	}

	@Test
	void parseAndSaveConfig_savesActivateSnykCode() throws Exception {
		String json = "{\"activateSnykCode\": true}";

		invokeParseAndSaveConfig(json);

		assertEquals("true", prefs.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY));
	}

	@Test
	void parseAndSaveConfig_savesActivateSnykIac() throws Exception {
		String json = "{\"activateSnykIac\": false}";

		invokeParseAndSaveConfig(json);

		assertEquals("false", prefs.getPref(Preferences.ACTIVATE_SNYK_IAC));
	}

	@Test
	void parseAndSaveConfig_savesOrganization() throws Exception {
		String json = "{\"organization\": \"my-org\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("my-org", prefs.getPref(Preferences.ORGANIZATION_KEY));
	}

	@Test
	void parseAndSaveConfig_savesEndpoint() throws Exception {
		String json = "{\"endpoint\": \"https://custom.api.snyk.io\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("https://custom.api.snyk.io", prefs.getEndpoint());
	}

	@Test
	void parseAndSaveConfig_savesInsecure() throws Exception {
		String json = "{\"insecure\": true}";

		invokeParseAndSaveConfig(json);

		assertTrue(prefs.isInsecure());
	}

	@Test
	void parseAndSaveConfig_savesScanningModeAuto() throws Exception {
		String json = "{\"scanningMode\": \"auto\"}";

		invokeParseAndSaveConfig(json);

		assertTrue(prefs.getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC));
	}

	@Test
	void parseAndSaveConfig_savesScanningModeManual() throws Exception {
		String json = "{\"scanningMode\": \"manual\"}";

		invokeParseAndSaveConfig(json);

		assertFalse(prefs.getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC));
	}

	@Test
	void parseAndSaveConfig_savesAuthenticationMethodOAuth() throws Exception {
		String json = "{\"authenticationMethod\": \"oauth\"}";

		invokeParseAndSaveConfig(json);

		assertEquals(AuthConstants.AUTH_OAUTH2, prefs.getPref(Preferences.AUTHENTICATION_METHOD));
	}

	@Test
	void parseAndSaveConfig_savesAuthenticationMethodToken() throws Exception {
		String json = "{\"authenticationMethod\": \"token\"}";

		invokeParseAndSaveConfig(json);

		assertEquals(AuthConstants.AUTH_API_TOKEN, prefs.getPref(Preferences.AUTHENTICATION_METHOD));
	}

	@Test
	void parseAndSaveConfig_savesSeverityFilters() throws Exception {
		String json = "{\"filterSeverity\": {\"critical\": false, \"high\": true, \"medium\": false, \"low\": true}}";

		invokeParseAndSaveConfig(json);

		assertEquals("false", prefs.getPref(Preferences.FILTER_SHOW_CRITICAL));
		assertEquals("true", prefs.getPref(Preferences.FILTER_SHOW_HIGH));
		assertEquals("false", prefs.getPref(Preferences.FILTER_SHOW_MEDIUM));
		assertEquals("true", prefs.getPref(Preferences.FILTER_SHOW_LOW));
	}

	@Test
	void parseAndSaveConfig_savesIssueViewOptions() throws Exception {
		String json = "{\"issueViewOptions\": {\"openIssues\": false, \"ignoredIssues\": true}}";

		invokeParseAndSaveConfig(json);

		assertEquals("false", prefs.getPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES));
		assertEquals("true", prefs.getPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES));
	}

	@Test
	void parseAndSaveConfig_savesEnableDeltaFindings() throws Exception {
		String json = "{\"enableDeltaFindings\": true}";

		invokeParseAndSaveConfig(json);

		assertEquals("true", prefs.getPref(Preferences.ENABLE_DELTA));
	}

	@Test
	void parseAndSaveConfig_savesSendErrorReports() throws Exception {
		String json = "{\"sendErrorReports\": false}";

		invokeParseAndSaveConfig(json);

		assertEquals("false", prefs.getPref(Preferences.SEND_ERROR_REPORTS));
	}

	@Test
	void parseAndSaveConfig_savesEnableTelemetry() throws Exception {
		String json = "{\"enableTelemetry\": false}";

		invokeParseAndSaveConfig(json);

		assertEquals("false", prefs.getPref(Preferences.ENABLE_TELEMETRY));
	}

	@Test
	void parseAndSaveConfig_handlesMultipleSettings() throws Exception {
		String json = "{\"cliPath\": \"/custom/path\", \"organization\": \"test-org\", \"insecure\": true}";

		invokeParseAndSaveConfig(json);

		assertEquals("/custom/path", prefs.getCliPath());
		assertEquals("test-org", prefs.getPref(Preferences.ORGANIZATION_KEY));
		assertTrue(prefs.isInsecure());
	}

	@Test
	void parseAndSaveConfig_ignoresUnknownFields() throws Exception {
		String json = "{\"unknownField\": \"value\", \"cliPath\": \"/valid/path\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("/valid/path", prefs.getCliPath());
	}

	@Test
	void parseAndSaveConfig_handlesEmptyJson() throws Exception {
		String json = "{}";

		invokeParseAndSaveConfig(json);

		// Should not throw, preferences should remain unchanged
	}

	@Test
	void isModified_returnsFalseInitially() {
		assertFalse(page.isModified());
	}

	private void invokeParseAndSaveConfig(String json) throws Exception {
		Method method = HTMLSettingsPreferencePage.class.getDeclaredMethod("parseAndSaveConfig", String.class);
		method.setAccessible(true);
		method.invoke(page, json);
	}
}
