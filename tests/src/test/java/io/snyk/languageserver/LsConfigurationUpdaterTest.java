package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.google.gson.Gson;

import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import io.snyk.languageserver.protocolextension.messageObjects.ConfigSetting;
import io.snyk.languageserver.protocolextension.messageObjects.LspConfigurationParam;
import io.snyk.languageserver.protocolextension.messageObjects.LspFolderConfig;

class LsConfigurationUpdaterTest {
	private Preferences preferenceMock;
	private final Gson gson = new Gson();

	@BeforeEach
	protected void setUp() {
		preferenceMock = mock(Preferences.class);
		PreferencesUtils.setPreferences(preferenceMock);

		FolderConfigSettings.setInstance(new FolderConfigSettings());
	}

	@Test
	void testBuildConfigurationParam_returnsLspConfigurationParam() {
		setupPreferenceMock();
		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertNotNull(param);
		assertNotNull(param.getSettings());
		assertNotNull(param.getFolderConfigs());
	}

	@Test
	void testBuildConfigurationParam_containsMachineSettings() {
		setupPreferenceMock();
		var param = new LsConfigurationUpdater().buildConfigurationParam();
		Map<String, ConfigSetting> settings = param.getSettings();

		assertNotNull(settings.get(LsKey.ENDPOINT.key));
		assertEquals("endpoint", settings.get(LsKey.ENDPOINT.key).getValue());

		assertNotNull(settings.get(LsKey.ORGANIZATION.key));
		assertEquals("organization", settings.get(LsKey.ORGANIZATION.key).getValue());
	}

	@Test
	void testBuildConfigurationParam_changedFlagFromExplicitChanges() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.ENDPOINT_KEY)).thenReturn(true);
		when(preferenceMock.isExplicitlyChanged(Preferences.ORGANIZATION_KEY)).thenReturn(false);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		Map<String, ConfigSetting> settings = param.getSettings();

		assertTrue(settings.get(LsKey.ENDPOINT.key).getChanged());
		assertFalse(settings.get(LsKey.ORGANIZATION.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_tokenChangedFlagFromExplicitChanges() {
		setupPreferenceMock();

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting tokenSetting = param.getSettings().get(LsKey.TOKEN.key);

		assertTrue(tokenSetting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_tokenAlwaysChanged() {
		setupPreferenceMock();

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting tokenSetting = param.getSettings().get(LsKey.TOKEN.key);

		assertTrue(tokenSetting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_outboundHasNoLockMetadata() {
		setupPreferenceMock();
		var param = new LsConfigurationUpdater().buildConfigurationParam();
		Map<String, ConfigSetting> settings = param.getSettings();

		for (ConfigSetting setting : settings.values()) {
			assertNull(setting.getSource());
			assertNull(setting.getOriginScope());
			assertNull(setting.getIsLocked());
		}
	}

	@Test
	void testBuildConfigurationParam_fullStateSemanticsAllSettingsIncluded() {
		setupPreferenceMock();
		var param = new LsConfigurationUpdater().buildConfigurationParam();
		Map<String, ConfigSetting> settings = param.getSettings();

		assertTrue(settings.containsKey(LsKey.ENDPOINT.key));
		assertTrue(settings.containsKey(LsKey.ORGANIZATION.key));
		assertTrue(settings.containsKey(LsKey.ACTIVATE_SNYK_CODE.key));
		assertTrue(settings.containsKey(LsKey.ACTIVATE_SNYK_OPEN_SOURCE.key));
		assertTrue(settings.containsKey(LsKey.ACTIVATE_SNYK_IAC.key));
		assertTrue(settings.containsKey(LsKey.ACTIVATE_SNYK_SECRETS.key));
		assertTrue(settings.containsKey(LsKey.INSECURE.key));
		assertTrue(settings.containsKey(LsKey.ADDITIONAL_PARAMS.key));
		assertTrue(settings.containsKey(LsKey.SCANNING_MODE.key));
	}

	@Test
	void testBuildConfigurationParam_resetToDefault_sendsNullValueWithChangedTrue() {
		setupPreferenceMock();
		when(preferenceMock.getPref(Preferences.ENDPOINT_KEY, Preferences.DEFAULT_ENDPOINT)).thenReturn(null);
		when(preferenceMock.isExplicitlyChanged(Preferences.ENDPOINT_KEY)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting endpointSetting = param.getSettings().get(LsKey.ENDPOINT.key);

		assertNull(endpointSetting.getValue());
		assertTrue(endpointSetting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_afterGlobalReset_doesNotReassertOverride() {
		// Re-push guard: after an inbound global reset clears explicit-changed for an
		// org-scope global key, the next outbound config must send changed=false so the
		// IDE does not re-assert the (now removed) user:global override. This replaces
		// the previous manual-restart verification.
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_OPEN_SOURCE)).thenReturn(false);
		when(preferenceMock.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_CODE_SECURITY)).thenReturn(false);
		when(preferenceMock.isExplicitlyChanged(Preferences.SCANNING_MODE_AUTOMATIC)).thenReturn(false);
		when(preferenceMock.isExplicitlyChanged(Preferences.RISK_SCORE_THRESHOLD)).thenReturn(false);

		var settings = new LsConfigurationUpdater().buildConfigurationParam().getSettings();

		assertFalse(settings.get(LsKey.ACTIVATE_SNYK_OPEN_SOURCE.key).getChanged());
		assertFalse(settings.get(LsKey.ACTIVATE_SNYK_CODE.key).getChanged());
		assertFalse(settings.get(LsKey.SCANNING_MODE.key).getChanged());
		assertFalse(settings.get(LsKey.RISK_SCORE_THRESHOLD.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_pendingResetEmitsNullChangedTrueOnce() {
		// Form-save queued a reset for ENDPOINT_KEY. First push must emit
		// {value:null, changed:true} — the only signal that makes snyk-ls Unset its
		// user:global override — overriding the normal per-key emit.
		setupPreferenceMock();
		when(preferenceMock.consumePendingResets())
				.thenReturn(java.util.Set.of(Preferences.ENDPOINT_KEY))
				.thenReturn(java.util.Set.of());

		var first = new LsConfigurationUpdater().buildConfigurationParam()
				.getSettings().get(LsKey.ENDPOINT.key);
		assertNull(first.getValue());
		assertTrue(first.getChanged());

		// Re-push guard: queue drained, so the next push emits the normal value with
		// changed=false (not explicitly changed), never re-asserting null/changed.
		var second = new LsConfigurationUpdater().buildConfigurationParam()
				.getSettings().get(LsKey.ENDPOINT.key);
		assertEquals("endpoint", second.getValue());
		assertFalse(second.getChanged());
	}

	@Test
	void testBuildConfigurationParam_unchangedSettingHasChangedFalse() {
		setupPreferenceMock();
		when(preferenceMock.getPref(Preferences.ENDPOINT_KEY, Preferences.DEFAULT_ENDPOINT)).thenReturn(Preferences.DEFAULT_ENDPOINT);
		when(preferenceMock.isExplicitlyChanged(Preferences.ENDPOINT_KEY)).thenReturn(false);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting endpointSetting = param.getSettings().get(LsKey.ENDPOINT.key);

		assertFalse(endpointSetting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_folderConfigsFromFolderConfigSettings() {
		setupPreferenceMock();

		String folderJson = """
				{
					"folderPath": "/test/project",
					"settings": {
						"base_branch": {"value": "main", "changed": false}
					}
				}
				""";
		LspFolderConfig folderConfig = gson.fromJson(folderJson, LspFolderConfig.class);
		FolderConfigSettings folderConfigSettings = new FolderConfigSettings();
		folderConfigSettings.addFolderConfig(folderConfig);
		FolderConfigSettings.setInstance(folderConfigSettings);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		List<LspFolderConfig> folderConfigs = param.getFolderConfigs();

		assertEquals(1, folderConfigs.size());
	}

	@Test
	void testRoundTrip_userChangeProducesCorrectPayload() {
		setupPreferenceMock();
		when(preferenceMock.getPref(Preferences.ENDPOINT_KEY, Preferences.DEFAULT_ENDPOINT)).thenReturn("https://custom.api.snyk.io");
		when(preferenceMock.isExplicitlyChanged(Preferences.ENDPOINT_KEY)).thenReturn(true);
		when(preferenceMock.getPref(Preferences.ORGANIZATION_KEY, "")).thenReturn("my-org");
		when(preferenceMock.isExplicitlyChanged(Preferences.ORGANIZATION_KEY)).thenReturn(true);
		when(preferenceMock.isExplicitlyChanged(Preferences.INSECURE_KEY)).thenReturn(true);

		String folderJson = """
				{
					"folderPath": "/workspace/project-a",
					"settings": {
						"base_branch": {"value": "develop", "changed": false, "source": "cli", "originScope": "folder"}
					}
				}
				""";
		LspFolderConfig folderConfig = gson.fromJson(folderJson, LspFolderConfig.class);
		LspFolderConfig updatedFolder = folderConfig.withSetting("base_branch", "main", true);
		FolderConfigSettings folderConfigSettings = new FolderConfigSettings();
		folderConfigSettings.addFolderConfig(updatedFolder);
		FolderConfigSettings.setInstance(folderConfigSettings);

		var param = new LsConfigurationUpdater().buildConfigurationParam();

		// Machine-scope settings with changed=true
		assertTrue(param.getSettings().get(LsKey.ENDPOINT.key).getChanged());
		assertEquals("https://custom.api.snyk.io", param.getSettings().get(LsKey.ENDPOINT.key).getValue());
		assertTrue(param.getSettings().get(LsKey.ORGANIZATION.key).getChanged());
		assertEquals("my-org", param.getSettings().get(LsKey.ORGANIZATION.key).getValue());

		// Folder config with user override
		assertEquals(1, param.getFolderConfigs().size());
		LspFolderConfig fc = param.getFolderConfigs().get(0);
		ConfigSetting baseBranch = fc.getSettings().get("base_branch");
		assertEquals("main", baseBranch.getValue());
		assertTrue(baseBranch.getChanged());
		assertEquals("cli", baseBranch.getSource());
		assertEquals("folder", baseBranch.getOriginScope());

		assertTrue(param.getSettings().get(LsKey.INSECURE.key).getChanged());

		// No lock metadata on machine-scope settings
		assertNull(param.getSettings().get(LsKey.ENDPOINT.key).getSource());
		assertNull(param.getSettings().get(LsKey.ENDPOINT.key).getIsLocked());

		// Metadata fields are at the top level, not in settings map
		assertNotNull(param.getIntegrationName());
		assertNotNull(param.getRequiredProtocolVersion());
		assertNotNull(param.getTrustedFolders());

		// Serialize to verify it's valid JSON structure
		String json = gson.toJson(param);
		LspConfigurationParam deserialized = gson.fromJson(json, LspConfigurationParam.class);
		assertNotNull(deserialized.getSettings());
		assertNotNull(deserialized.getFolderConfigs());
	}

	@Test
	void testBuildConfigurationParam_snykSecretsEnabledPresentAndChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_SECRETS)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting setting = param.getSettings().get(LsKey.ACTIVATE_SNYK_SECRETS.key);

		assertNotNull(setting);
		assertTrue(setting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_sendErrorReportsChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.SEND_ERROR_REPORTS)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsKey.SEND_ERROR_REPORTS.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_manageBinariesChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.MANAGE_BINARIES_AUTOMATICALLY)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsKey.MANAGE_BINARIES_AUTOMATICALLY.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_cliPathChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.CLI_PATH)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsKey.CLI_PATH.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_userSettingsPath_outbound() {
		setupPreferenceMock();
		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertNotNull(param.getSettings().get(LsKey.USER_SETTINGS_PATH.key));
		assertEquals("path", param.getSettings().get(LsKey.USER_SETTINGS_PATH.key).getValue());
	}

	@Test
	void testBuildConfigurationParam_userSettingsPath_changedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.PATH_KEY)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsKey.USER_SETTINGS_PATH.key).getChanged());
	}

	@Test
	void testLsSettingsRegistry_userSettingsPath_inboundRoundTrip() {
		var entry = LsSettingsRegistry.BY_LS_KEY.get(LsKey.USER_SETTINGS_PATH.key);
		assertNotNull(entry, "BY_LS_KEY must contain user_settings_path");
		assertEquals(Preferences.PATH_KEY, entry.prefKey);
		assertEquals("/usr/local/bin", entry.inboundDeserializer.apply("/usr/local/bin"));
	}

	@Test
	void testBuildConfigurationParam_cliBaseDownloadUrlChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.CLI_BASE_URL)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsKey.CLI_BASE_DOWNLOAD_URL.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_authenticationMethodChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.AUTHENTICATION_METHOD)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsKey.AUTHENTICATION_METHOD.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_enableDeltaChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.ENABLE_DELTA)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsKey.ENABLE_DELTA_FINDINGS.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_riskScoreThresholdChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.RISK_SCORE_THRESHOLD)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsKey.RISK_SCORE_THRESHOLD.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_issueViewOpenIssuesChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsKey.ISSUE_VIEW_OPEN_ISSUES.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_issueViewIgnoredIssuesUnchangedWhenNotExplicitlyChanged() {
		setupPreferenceMock();

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertFalse(param.getSettings().get(LsKey.ISSUE_VIEW_IGNORED_ISSUES.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_severityFilterSentAsIndividualKeys() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.FILTER_SHOW_CRITICAL)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		Map<String, ConfigSetting> settings = param.getSettings();

		// LS expects individual boolean keys, not a composite "enabled_severities" object
		assertNotNull(settings.get(LsKey.SEVERITY_FILTER_CRITICAL.key));
		assertNotNull(settings.get(LsKey.SEVERITY_FILTER_HIGH.key));
		assertNotNull(settings.get(LsKey.SEVERITY_FILTER_MEDIUM.key));
		assertNotNull(settings.get(LsKey.SEVERITY_FILTER_LOW.key));

		assertEquals(Boolean.TRUE, settings.get(LsKey.SEVERITY_FILTER_CRITICAL.key).getValue());
		assertEquals(Boolean.FALSE, settings.get(LsKey.SEVERITY_FILTER_HIGH.key).getValue());
		assertEquals(Boolean.TRUE, settings.get(LsKey.SEVERITY_FILTER_MEDIUM.key).getValue());
		assertEquals(Boolean.FALSE, settings.get(LsKey.SEVERITY_FILTER_LOW.key).getValue());
	}

	@Test
	void testBuildConfigurationParam_severityFilterChangedWhenAnySeverityChanged() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.FILTER_SHOW_LOW)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		// All 4 severity keys share the anySeverityChanged flag
		assertTrue(param.getSettings().get(LsKey.SEVERITY_FILTER_LOW.key).getChanged());
		assertTrue(param.getSettings().get(LsKey.SEVERITY_FILTER_CRITICAL.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_severityFilterUnchangedWhenNoSeverityChanged() {
		setupPreferenceMock();

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertFalse(param.getSettings().get(LsKey.SEVERITY_FILTER_CRITICAL.key).getChanged());
		assertFalse(param.getSettings().get(LsKey.SEVERITY_FILTER_HIGH.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_noCompositeEnabledSeveritiesKey() {
		setupPreferenceMock();

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		Map<String, ConfigSetting> settings = param.getSettings();
		assertFalse(settings.containsKey("enabled_severities"),
				"LS has no 'enabled_severities' key; individual severity_filter_* keys required");
	}

	@Test
	void testBuildConfigurationParam_metadataFieldsAreTopLevel() {
		setupPreferenceMock();

		var param = new LsConfigurationUpdater().buildConfigurationParam();

		assertNotNull(param.getIntegrationName());
		assertNotNull(param.getIntegrationVersion());
		assertNotNull(param.getRequiredProtocolVersion());
		assertNotNull(param.getRuntimeName());
		assertNotNull(param.getRuntimeVersion());
		assertNotNull(param.getOsArch());
		assertNotNull(param.getOsPlatform());
		assertNotNull(param.getTrustedFolders());
	}

	@Test
	void testBuildConfigurationParam_settingsUseSnakeCaseKeys() {
		setupPreferenceMock();
		var param = new LsConfigurationUpdater().buildConfigurationParam();
		Map<String, ConfigSetting> settings = param.getSettings();

		// Verify settings keys are snake_case
		assertTrue(settings.containsKey("api_endpoint"));
		assertTrue(settings.containsKey("cli_path"));
		assertTrue(settings.containsKey("proxy_insecure"));
		assertTrue(settings.containsKey("snyk_code_enabled"));
		assertTrue(settings.containsKey("snyk_oss_enabled"));
		assertTrue(settings.containsKey("snyk_iac_enabled"));
		assertTrue(settings.containsKey("scan_automatic"));
		assertTrue(settings.containsKey("automatic_download"));
		assertTrue(settings.containsKey("binary_base_url"));
		assertTrue(settings.containsKey("scan_net_new"));
		assertTrue(settings.containsKey("severity_filter_critical"));
		assertTrue(settings.containsKey("severity_filter_high"));
		assertTrue(settings.containsKey("severity_filter_medium"));
		assertTrue(settings.containsKey("severity_filter_low"));
		assertTrue(settings.containsKey("issue_view_open_issues"));
	}

	@Test
	void testBuildConfigurationParam_highRiskKeySerializedValues() {
		setupPreferenceMock();

		// risk_score_threshold: "200" pref → Integer(200), "0" → null
		when(preferenceMock.getPref(Preferences.RISK_SCORE_THRESHOLD, "0")).thenReturn("200");
		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertEquals(Integer.valueOf(200), param.getSettings().get(LsKey.RISK_SCORE_THRESHOLD.key).getValue(),
				"risk_score_threshold must serialize as Integer, not String");

		when(preferenceMock.getPref(Preferences.RISK_SCORE_THRESHOLD, "0")).thenReturn("0");
		param = new LsConfigurationUpdater().buildConfigurationParam();
		assertNull(param.getSettings().get(LsKey.RISK_SCORE_THRESHOLD.key).getValue(),
				"risk_score_threshold '0' must serialize as null (no threshold)");

		// scan_automatic: pref "true" → Boolean.TRUE, "false" → Boolean.FALSE
		when(preferenceMock.getPref(Preferences.SCANNING_MODE_AUTOMATIC, "true")).thenReturn("true");
		param = new LsConfigurationUpdater().buildConfigurationParam();
		assertEquals(Boolean.TRUE, param.getSettings().get(LsKey.SCANNING_MODE.key).getValue(),
				"scan_automatic must serialize as Boolean, not String");

		when(preferenceMock.getPref(Preferences.SCANNING_MODE_AUTOMATIC, "true")).thenReturn("false");
		param = new LsConfigurationUpdater().buildConfigurationParam();
		assertEquals(Boolean.FALSE, param.getSettings().get(LsKey.SCANNING_MODE.key).getValue(),
				"scan_automatic false must serialize as Boolean.FALSE");
	}

	// --- IDE-2149: pending-reset emit + re-push guard for every resettable org-scope key. ---
	// prefKey -> ls key. Mirrors the resettable set covered form-side in HTMLSettingsPreferencePageTest.
	private static final String[][] RESETTABLE = {
			{Preferences.ACTIVATE_SNYK_OPEN_SOURCE, LsKey.ACTIVATE_SNYK_OPEN_SOURCE.key},
			{Preferences.ACTIVATE_SNYK_CODE_SECURITY, LsKey.ACTIVATE_SNYK_CODE.key},
			{Preferences.ACTIVATE_SNYK_IAC, LsKey.ACTIVATE_SNYK_IAC.key},
			{Preferences.ACTIVATE_SNYK_SECRETS, LsKey.ACTIVATE_SNYK_SECRETS.key},
			{Preferences.SCANNING_MODE_AUTOMATIC, LsKey.SCANNING_MODE.key},
			{Preferences.ENABLE_DELTA, LsKey.ENABLE_DELTA_FINDINGS.key},
			{Preferences.FILTER_SHOW_CRITICAL, LsKey.SEVERITY_FILTER_CRITICAL.key},
			{Preferences.FILTER_SHOW_HIGH, LsKey.SEVERITY_FILTER_HIGH.key},
			{Preferences.FILTER_SHOW_MEDIUM, LsKey.SEVERITY_FILTER_MEDIUM.key},
			{Preferences.FILTER_SHOW_LOW, LsKey.SEVERITY_FILTER_LOW.key},
			{Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, LsKey.ISSUE_VIEW_OPEN_ISSUES.key},
			{Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, LsKey.ISSUE_VIEW_IGNORED_ISSUES.key},
			{Preferences.RISK_SCORE_THRESHOLD, LsKey.RISK_SCORE_THRESHOLD.key},
			{Preferences.ORGANIZATION_KEY, LsKey.ORGANIZATION.key},
	};

	@Test
	void testBuildConfigurationParam_pendingResetEmitsNullChangedTrueOnce_forEachResettableKey() {
		for (String[] row : RESETTABLE) {
			String prefKey = row[0], lsKey = row[1];
			setUp();
			setupPreferenceMock();
			// First push: this key is queued for reset; second push: queue drained.
			when(preferenceMock.consumePendingResets())
					.thenReturn(java.util.Set.of(prefKey))
					.thenReturn(java.util.Set.of());
			// After reset the override is gone and tracking cleared -> not explicitly changed.
			when(preferenceMock.isExplicitlyChanged(prefKey)).thenReturn(false);

			var first = new LsConfigurationUpdater().buildConfigurationParam().getSettings().get(lsKey);
			assertNull(first.getValue(), lsKey + " reset must emit value=null");
			assertTrue(first.getChanged(), lsKey + " reset must emit changed=true");

			// Re-push guard: queue drained -> normal value, changed=false. Never re-asserts null/changed.
			var second = new LsConfigurationUpdater().buildConfigurationParam().getSettings().get(lsKey);
			assertNotNull(second.getValue(), lsKey + " second push must send the concrete default, not null");
			assertFalse(second.getChanged(), lsKey + " second push must send changed=false");
		}
	}

	@Test
	void testBuildConfigurationParam_pendingResetAllSeveritiesTogether() {
		setupPreferenceMock();
		var all = java.util.Set.of(Preferences.FILTER_SHOW_CRITICAL, Preferences.FILTER_SHOW_HIGH,
				Preferences.FILTER_SHOW_MEDIUM, Preferences.FILTER_SHOW_LOW);
		when(preferenceMock.consumePendingResets()).thenReturn(all).thenReturn(java.util.Set.of());

		var first = new LsConfigurationUpdater().buildConfigurationParam().getSettings();
		for (LsKey k : new LsKey[] {LsKey.SEVERITY_FILTER_CRITICAL, LsKey.SEVERITY_FILTER_HIGH,
				LsKey.SEVERITY_FILTER_MEDIUM, LsKey.SEVERITY_FILTER_LOW}) {
			assertNull(first.get(k.key).getValue(), k.key + " must emit null on reset");
			assertTrue(first.get(k.key).getChanged(), k.key + " must emit changed=true on reset");
		}

		// Second push: queue drained. No severity is explicitly changed -> all changed=false.
		var second = new LsConfigurationUpdater().buildConfigurationParam().getSettings();
		for (LsKey k : new LsKey[] {LsKey.SEVERITY_FILTER_CRITICAL, LsKey.SEVERITY_FILTER_HIGH,
				LsKey.SEVERITY_FILTER_MEDIUM, LsKey.SEVERITY_FILTER_LOW}) {
			assertFalse(second.get(k.key).getChanged(), k.key + " re-push must be changed=false");
		}
	}

	@Test
	void testBuildConfigurationParam_mixedBatch_resetsSomeKeepsOthers() {
		setupPreferenceMock();
		// oss + risk_score are reset; secrets stays a concrete explicit change.
		when(preferenceMock.consumePendingResets())
				.thenReturn(java.util.Set.of(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, Preferences.RISK_SCORE_THRESHOLD))
				.thenReturn(java.util.Set.of());
		when(preferenceMock.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_OPEN_SOURCE)).thenReturn(false);
		when(preferenceMock.isExplicitlyChanged(Preferences.RISK_SCORE_THRESHOLD)).thenReturn(false);
		when(preferenceMock.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_SECRETS)).thenReturn(true);

		var first = new LsConfigurationUpdater().buildConfigurationParam().getSettings();
		// Reset keys: null + changed=true.
		assertNull(first.get(LsKey.ACTIVATE_SNYK_OPEN_SOURCE.key).getValue());
		assertTrue(first.get(LsKey.ACTIVATE_SNYK_OPEN_SOURCE.key).getChanged());
		assertNull(first.get(LsKey.RISK_SCORE_THRESHOLD.key).getValue());
		assertTrue(first.get(LsKey.RISK_SCORE_THRESHOLD.key).getChanged());
		// Concrete key: real value + changed=true.
		assertNotNull(first.get(LsKey.ACTIVATE_SNYK_SECRETS.key).getValue());
		assertTrue(first.get(LsKey.ACTIVATE_SNYK_SECRETS.key).getChanged());

		var second = new LsConfigurationUpdater().buildConfigurationParam().getSettings();
		// Reset keys now changed=false; concrete key still changed=true.
		assertFalse(second.get(LsKey.ACTIVATE_SNYK_OPEN_SOURCE.key).getChanged());
		assertFalse(second.get(LsKey.RISK_SCORE_THRESHOLD.key).getChanged());
		assertTrue(second.get(LsKey.ACTIVATE_SNYK_SECRETS.key).getChanged());
	}

	@Test
	void testBuildConfigurationParam_exactlyOnceAcrossAllResetKeys() {
		setupPreferenceMock();
		java.util.Set<String> allPrefKeys = new java.util.HashSet<>();
		for (String[] row : RESETTABLE) {
			allPrefKeys.add(row[0]);
			when(preferenceMock.isExplicitlyChanged(row[0])).thenReturn(false);
		}
		when(preferenceMock.consumePendingResets()).thenReturn(allPrefKeys).thenReturn(java.util.Set.of());

		var first = new LsConfigurationUpdater().buildConfigurationParam().getSettings();
		for (String[] row : RESETTABLE) {
			assertNull(first.get(row[1]).getValue(), row[1] + " must emit null on first push");
			assertTrue(first.get(row[1]).getChanged(), row[1] + " must emit changed=true on first push");
		}

		// Exactly-once: after drain, EVERY reset key re-pushes changed=false in one go.
		var second = new LsConfigurationUpdater().buildConfigurationParam().getSettings();
		for (String[] row : RESETTABLE) {
			assertFalse(second.get(row[1]).getChanged(), row[1] + " must not re-assert reset on second push");
		}
	}

	private void setupPreferenceMock() {
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_IAC, "true")).thenReturn("iac");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_IAC, "false")).thenReturn("iac");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_IAC, "")).thenReturn("iac");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY, "true")).thenReturn("code");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY, "false")).thenReturn("code");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY, "")).thenReturn("code");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "true")).thenReturn("oss");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "false")).thenReturn("oss");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "")).thenReturn("oss");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_SECRETS, "true")).thenReturn("false");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_SECRETS, "false")).thenReturn("false");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_SECRETS, "")).thenReturn("false");
		when(preferenceMock.getPref(Preferences.INSECURE_KEY, "")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.INSECURE_KEY, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.INSECURE_KEY, "false")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ENDPOINT_KEY, Preferences.DEFAULT_ENDPOINT)).thenReturn("endpoint");
		when(preferenceMock.getPref(Preferences.ADDITIONAL_PARAMETERS, "")).thenReturn("addParams");
		when(preferenceMock.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "")).thenReturn("a=b;c=d");
		when(preferenceMock.getPref(Preferences.PATH_KEY, "")).thenReturn("path");
		when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "false")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "false")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ORGANIZATION_KEY, "")).thenReturn("organization");
		final var path = "/usr/local/bin/snyk";
		when(preferenceMock.getCliPath()).thenReturn(path);
		when(preferenceMock.getPref(Preferences.AUTH_TOKEN_KEY, "")).thenReturn("my-token");
		when(preferenceMock.getPref(Preferences.AUTHENTICATION_METHOD, AuthConstants.AUTH_OAUTH2)).thenReturn("oauth");
		when(preferenceMock.getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC)).thenReturn(true);
		when(preferenceMock.getPref(Preferences.ENABLE_DELTA, Boolean.FALSE.toString())).thenReturn("true");
		when(preferenceMock.getPref(Preferences.RISK_SCORE_THRESHOLD, "0")).thenReturn("200");
		when(preferenceMock.getReleaseChannel()).thenReturn("stable");
		when(preferenceMock.getPref(Preferences.DEVICE_ID, "")).thenReturn("test-device-id");
		when(preferenceMock.getPref(Preferences.TRUSTED_FOLDERS)).thenReturn("");
		when(preferenceMock.getPref(Preferences.CLI_BASE_URL, "https://downloads.snyk.io")).thenReturn("https://downloads.snyk.io");

		// Severity filter mocks
		when(preferenceMock.getBooleanPref(Preferences.FILTER_SHOW_CRITICAL, true)).thenReturn(true);
		when(preferenceMock.getBooleanPref(Preferences.FILTER_SHOW_HIGH, true)).thenReturn(false);
		when(preferenceMock.getBooleanPref(Preferences.FILTER_SHOW_MEDIUM, true)).thenReturn(true);
		when(preferenceMock.getBooleanPref(Preferences.FILTER_SHOW_LOW, true)).thenReturn(false);
		when(preferenceMock.getPref(Preferences.FILTER_SHOW_CRITICAL, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.FILTER_SHOW_HIGH, "true")).thenReturn("false");
		when(preferenceMock.getPref(Preferences.FILTER_SHOW_MEDIUM, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.FILTER_SHOW_LOW, "true")).thenReturn("false");

		// Issue view options mocks
		when(preferenceMock.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, true)).thenReturn(true);
		when(preferenceMock.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, false)).thenReturn(false);
		when(preferenceMock.getPref(Preferences.SCANNING_MODE_AUTOMATIC, "true")).thenReturn("true");
	}
}
