package io.snyk.eclipse.plugin.preferences;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import io.snyk.languageserver.protocolextension.messageObjects.ConfigSetting;
import io.snyk.languageserver.protocolextension.messageObjects.LspFolderConfig;

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
		String json = "{\"cli_path\": \"/usr/local/bin/snyk\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("/usr/local/bin/snyk", prefs.getCliPath());
	}

	@Test
	void parseAndSaveConfig_savesManageBinariesAutomatically() throws Exception {
		String json = "{\"automatic_download\": false}";

		invokeParseAndSaveConfig(json);

		assertFalse(prefs.isManagedBinaries());
	}

	@Test
	void parseAndSaveConfig_savesCliBaseDownloadURL() throws Exception {
		String json = "{\"binary_base_url\": \"https://custom.downloads.snyk.io\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("https://custom.downloads.snyk.io", prefs.getPref(Preferences.CLI_BASE_URL));
	}

	@Test
	void parseAndSaveConfig_savesCliReleaseChannel() throws Exception {
		String json = "{\"cli_release_channel\": \"preview\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("preview", prefs.getReleaseChannel());
	}

	@Test
	void parseAndSaveConfig_savesActivateSnykOpenSource() throws Exception {
		String json = "{\"snyk_oss_enabled\": false}";

		invokeParseAndSaveConfig(json);

		assertEquals("false", prefs.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
	}

	@Test
	void parseAndSaveConfig_savesActivateSnykCode() throws Exception {
		String json = "{\"snyk_code_enabled\": true}";

		invokeParseAndSaveConfig(json);

		assertEquals("true", prefs.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY));
	}

	@Test
	void parseAndSaveConfig_savesActivateSnykIac() throws Exception {
		String json = "{\"snyk_iac_enabled\": false}";

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
		String json = "{\"api_endpoint\": \"https://custom.api.snyk.io\"}";

		invokeParseAndSaveConfig(json);

		assertEquals("https://custom.api.snyk.io", prefs.getEndpoint());
	}

	@Test
	void parseAndSaveConfig_savesInsecure() throws Exception {
		String json = "{\"proxy_insecure\": true}";

		invokeParseAndSaveConfig(json);

		assertTrue(prefs.isInsecure());
	}

	@Test
	void parseAndSaveConfig_savesScanningModeAuto() throws Exception {
		String json = "{\"scan_automatic\": \"auto\"}";

		invokeParseAndSaveConfig(json);

		assertTrue(prefs.getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC));
	}

	@Test
	void parseAndSaveConfig_savesScanningModeManual() throws Exception {
		String json = "{\"scan_automatic\": \"manual\"}";

		invokeParseAndSaveConfig(json);

		assertFalse(prefs.getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC));
	}

	@Test
	void parseAndSaveConfig_savesAuthenticationMethodOAuth() throws Exception {
		String json = "{\"authentication_method\": \"oauth\"}";

		invokeParseAndSaveConfig(json);

		assertEquals(AuthConstants.AUTH_OAUTH2, prefs.getPref(Preferences.AUTHENTICATION_METHOD));
	}

	@Test
	void parseAndSaveConfig_savesAuthenticationMethodToken() throws Exception {
		String json = "{\"authentication_method\": \"token\"}";

		invokeParseAndSaveConfig(json);

		assertEquals(AuthConstants.AUTH_API_TOKEN, prefs.getPref(Preferences.AUTHENTICATION_METHOD));
	}

	@Test
	void parseAndSaveConfig_savesSeverityFilters() throws Exception {
		String json = "{\"severity_filter_critical\": false, \"severity_filter_high\": true, \"severity_filter_medium\": false, \"severity_filter_low\": true}";

		invokeParseAndSaveConfig(json);

		assertEquals("false", prefs.getPref(Preferences.FILTER_SHOW_CRITICAL));
		assertEquals("true", prefs.getPref(Preferences.FILTER_SHOW_HIGH));
		assertEquals("false", prefs.getPref(Preferences.FILTER_SHOW_MEDIUM));
		assertEquals("true", prefs.getPref(Preferences.FILTER_SHOW_LOW));
	}

	@Test
	void parseAndSaveConfig_savesIssueViewOptions() throws Exception {
		String json = "{\"issue_view_open_issues\": false, \"issue_view_ignored_issues\": true}";

		invokeParseAndSaveConfig(json);

		assertEquals("false", prefs.getPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES));
		assertEquals("true", prefs.getPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES));
	}

	@Test
	void parseAndSaveConfig_savesEnableDeltaFindings() throws Exception {
		String json = "{\"scan_net_new\": true}";

		invokeParseAndSaveConfig(json);

		assertEquals("true", prefs.getPref(Preferences.ENABLE_DELTA));
	}

	@Test
	void parseAndSaveConfig_savesRiskScoreThreshold() throws Exception {
		String json = "{\"risk_score_threshold\": 200}";

		invokeParseAndSaveConfig(json);

		assertEquals("200", prefs.getPref(Preferences.RISK_SCORE_THRESHOLD));
	}

	@Test
	void parseAndSaveConfig_handlesMultipleSettings() throws Exception {
		String json = "{\"cli_path\": \"/custom/path\", \"organization\": \"test-org\", \"proxy_insecure\": true}";

		invokeParseAndSaveConfig(json);

		assertEquals("/custom/path", prefs.getCliPath());
		assertEquals("test-org", prefs.getPref(Preferences.ORGANIZATION_KEY));
		assertTrue(prefs.isInsecure());
	}

	@Test
	void parseAndSaveConfig_ignoresUnknownFields() throws Exception {
		String json = "{\"unknownField\": \"value\", \"cli_path\": \"/valid/path\"}";

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
	void parseAndSaveConfig_nullEndpointClearsExplicitOverride() throws Exception {
		// User previously set endpoint
		prefs.storeAndTrackChange(Preferences.ENDPOINT_KEY, "https://user-override.snyk.io");
		assertTrue(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));

		// LS form sends null for endpoint (user reset it)
		String json = "{\"api_endpoint\": null}";
		invokeParseAndSaveConfig(json);

		// Override should be cleared
		assertFalse(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));
	}

	@Test
	void parseAndSaveConfig_nullOrganizationClearsExplicitOverride() throws Exception {
		prefs.storeAndTrackChange(Preferences.ORGANIZATION_KEY, "my-org");
		assertTrue(prefs.isExplicitlyChanged(Preferences.ORGANIZATION_KEY));

		String json = "{\"organization\": null}";
		invokeParseAndSaveConfig(json);

		assertFalse(prefs.isExplicitlyChanged(Preferences.ORGANIZATION_KEY));
	}

	@Test
	void parseAndSaveConfig_nullCliPathClearsExplicitOverride() throws Exception {
		prefs.storeAndTrackChange(Preferences.CLI_PATH, "/custom/cli");
		assertTrue(prefs.isExplicitlyChanged(Preferences.CLI_PATH));

		String json = "{\"cli_path\": null}";
		invokeParseAndSaveConfig(json);

		assertFalse(prefs.isExplicitlyChanged(Preferences.CLI_PATH));
	}

	@Test
	void parseAndSaveConfig_mixedNullAndValueFields() throws Exception {
		prefs.storeAndTrackChange(Preferences.ENDPOINT_KEY, "https://custom.snyk.io");
		prefs.storeAndTrackChange(Preferences.ORGANIZATION_KEY, "old-org");
		assertTrue(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));
		assertTrue(prefs.isExplicitlyChanged(Preferences.ORGANIZATION_KEY));

		// Endpoint reset (null), organization changed to new value
		String json = "{\"api_endpoint\": null, \"organization\": \"new-org\"}";
		invokeParseAndSaveConfig(json);

		assertFalse(prefs.isExplicitlyChanged(Preferences.ENDPOINT_KEY));
		assertTrue(prefs.isExplicitlyChanged(Preferences.ORGANIZATION_KEY));
		assertEquals("new-org", prefs.getPref(Preferences.ORGANIZATION_KEY));
	}

	@Test
	void parseAndSaveConfig_folderFieldSentAsNullBecomesReset() throws Exception {
		FolderConfigSettings.setInstance(new FolderConfigSettings());
		String folderPath = "/work/reset-project";
		// Org-scope folder fields sent as JSON null are resets; the IDE must emit
		// {value:null, changed:true} so snyk-ls Unsets the override.
		// additional_parameters (array), additional_environment (array) and scan_command_config
		// (object) are non-scalar fields; a JSON null must still hit the reset branch and emit
		// {value:null, changed:true}, not be skipped by any type-specific handling.
		String json = "{\"folderConfigs\": [{"
				+ "\"folderPath\": \"" + folderPath + "\","
				+ "\"snyk_code_enabled\": null,"
				+ "\"preferred_org\": null,"
				+ "\"risk_score_threshold\": null,"
				+ "\"additional_parameters\": null,"
				+ "\"additional_environment\": null,"
				+ "\"scan_command_config\": null"
				+ "}]}";

		invokeParseAndSaveConfig(json);

		LspFolderConfig stored = FolderConfigSettings.getInstance().getFolderConfig(folderPath);
		assertResetSetting(stored, "snyk_code_enabled");
		assertResetSetting(stored, "preferred_org");
		assertResetSetting(stored, "risk_score_threshold");
		assertResetSetting(stored, "additional_parameters");
		assertResetSetting(stored, "additional_environment");
		assertResetSetting(stored, "scan_command_config");
	}

	@Test
	void parseAndSaveConfig_folderNonNullFieldIsNotAReset() throws Exception {
		FolderConfigSettings.setInstance(new FolderConfigSettings());
		String folderPath = "/work/noreset-project";
		String json = "{\"folderConfigs\": [{"
				+ "\"folderPath\": \"" + folderPath + "\","
				+ "\"snyk_code_enabled\": false"
				+ "}]}";

		invokeParseAndSaveConfig(json);

		LspFolderConfig stored = FolderConfigSettings.getInstance().getFolderConfig(folderPath);
		ConfigSetting setting = stored.getSettings().get("snyk_code_enabled");
		assertNotNull(setting);
		assertEquals(Boolean.FALSE, setting.getValue());
		assertEquals(Boolean.TRUE, setting.getChanged());
	}

	@Test
	void parseAndSaveConfig_anyFolderFieldNullIsForwardedAsReset() throws Exception {
		FolderConfigSettings.setInstance(new FolderConfigSettings());
		String folderPath = "/work/basebranch-project";
		// The IDE no longer maintains a reset whitelist: every folder field sent as JSON null is
		// forwarded as {value:null, changed:true}. snyk-ls is authoritative and ignores nulls on
		// keys with no fallback layer, so forwarding e.g. base_branch is a safe no-op there.
		String json = "{\"folderConfigs\": [{"
				+ "\"folderPath\": \"" + folderPath + "\","
				+ "\"base_branch\": null"
				+ "}]}";

		invokeParseAndSaveConfig(json);

		LspFolderConfig stored = FolderConfigSettings.getInstance().getFolderConfig(folderPath);
		assertResetSetting(stored, "base_branch");
	}

	private static void assertResetSetting(LspFolderConfig stored, String key) {
		assertNotNull(stored.getSettings(), "folder settings should exist");
		ConfigSetting setting = stored.getSettings().get(key);
		assertNotNull(setting, key + " reset setting should be present");
		assertNull(setting.getValue(), key + " reset value should be null");
		assertEquals(Boolean.TRUE, setting.getChanged(), key + " reset changed should be true");
	}

	private void invokeParseAndSaveConfig(String json) throws Exception {
		Method method = HTMLSettingsPreferencePage.class.getDeclaredMethod("parseAndSaveConfig", String.class);
		method.setAccessible(true);
		method.invoke(page, json);
	}
}
