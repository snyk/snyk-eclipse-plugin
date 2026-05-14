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

		assertNotNull(settings.get(LsSettingsKeys.ENDPOINT));
		assertEquals("endpoint", settings.get(LsSettingsKeys.ENDPOINT).getValue());

		assertNotNull(settings.get(LsSettingsKeys.ORGANIZATION));
		assertEquals("organization", settings.get(LsSettingsKeys.ORGANIZATION).getValue());
	}

	@Test
	void testBuildConfigurationParam_changedFlagFromExplicitChanges() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.ENDPOINT_KEY)).thenReturn(true);
		when(preferenceMock.isExplicitlyChanged(Preferences.ORGANIZATION_KEY)).thenReturn(false);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		Map<String, ConfigSetting> settings = param.getSettings();

		assertTrue(settings.get(LsSettingsKeys.ENDPOINT).getChanged());
		assertFalse(settings.get(LsSettingsKeys.ORGANIZATION).getChanged());
	}

	@Test
	void testBuildConfigurationParam_tokenChangedFlagFromExplicitChanges() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.AUTH_TOKEN_KEY)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting tokenSetting = param.getSettings().get(LsSettingsKeys.TOKEN);

		assertTrue(tokenSetting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_tokenUnchangedWhenNotExplicitlyChanged() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.AUTH_TOKEN_KEY)).thenReturn(false);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting tokenSetting = param.getSettings().get(LsSettingsKeys.TOKEN);

		assertFalse(tokenSetting.getChanged());
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

		assertTrue(settings.containsKey(LsSettingsKeys.ENDPOINT));
		assertTrue(settings.containsKey(LsSettingsKeys.ORGANIZATION));
		assertTrue(settings.containsKey(LsSettingsKeys.ACTIVATE_SNYK_CODE));
		assertTrue(settings.containsKey(LsSettingsKeys.ACTIVATE_SNYK_OPEN_SOURCE));
		assertTrue(settings.containsKey(LsSettingsKeys.ACTIVATE_SNYK_IAC));
		assertTrue(settings.containsKey(LsSettingsKeys.ACTIVATE_SNYK_SECRETS));
		assertTrue(settings.containsKey(LsSettingsKeys.INSECURE));
		assertTrue(settings.containsKey(LsSettingsKeys.ADDITIONAL_PARAMS));
		assertTrue(settings.containsKey(LsSettingsKeys.SCANNING_MODE));
	}

	@Test
	void testBuildConfigurationParam_resetToDefault_sendsNullValueWithChangedTrue() {
		setupPreferenceMock();
		when(preferenceMock.getPref(Preferences.ENDPOINT_KEY, "")).thenReturn(null);
		when(preferenceMock.isExplicitlyChanged(Preferences.ENDPOINT_KEY)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting endpointSetting = param.getSettings().get(LsSettingsKeys.ENDPOINT);

		assertNull(endpointSetting.getValue());
		assertTrue(endpointSetting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_unchangedSettingHasChangedFalse() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.ENDPOINT_KEY)).thenReturn(false);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting endpointSetting = param.getSettings().get(LsSettingsKeys.ENDPOINT);

		assertNotNull(endpointSetting.getValue());
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
		when(preferenceMock.getPref(Preferences.ENDPOINT_KEY, "")).thenReturn("https://custom.api.snyk.io");
		when(preferenceMock.isExplicitlyChanged(Preferences.ENDPOINT_KEY)).thenReturn(true);
		when(preferenceMock.getPref(Preferences.ORGANIZATION_KEY, "")).thenReturn("my-org");
		when(preferenceMock.isExplicitlyChanged(Preferences.ORGANIZATION_KEY)).thenReturn(true);

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
		assertTrue(param.getSettings().get(LsSettingsKeys.ENDPOINT).getChanged());
		assertEquals("https://custom.api.snyk.io", param.getSettings().get(LsSettingsKeys.ENDPOINT).getValue());
		assertTrue(param.getSettings().get(LsSettingsKeys.ORGANIZATION).getChanged());
		assertEquals("my-org", param.getSettings().get(LsSettingsKeys.ORGANIZATION).getValue());

		// Folder config with user override
		assertEquals(1, param.getFolderConfigs().size());
		LspFolderConfig fc = param.getFolderConfigs().get(0);
		ConfigSetting baseBranch = fc.getSettings().get("base_branch");
		assertEquals("main", baseBranch.getValue());
		assertTrue(baseBranch.getChanged());
		assertEquals("cli", baseBranch.getSource());
		assertEquals("folder", baseBranch.getOriginScope());

		// Unchanged settings have changed=false
		assertFalse(param.getSettings().get(LsSettingsKeys.INSECURE).getChanged());

		// No lock metadata on machine-scope settings
		assertNull(param.getSettings().get(LsSettingsKeys.ENDPOINT).getSource());
		assertNull(param.getSettings().get(LsSettingsKeys.ENDPOINT).getIsLocked());

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
		ConfigSetting setting = param.getSettings().get(LsSettingsKeys.ACTIVATE_SNYK_SECRETS);

		assertNotNull(setting);
		assertTrue(setting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_additionalEnvChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.ADDITIONAL_ENVIRONMENT)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.ADDITIONAL_ENV).getChanged());
	}

	@Test
	void testBuildConfigurationParam_sendErrorReportsChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.SEND_ERROR_REPORTS)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.SEND_ERROR_REPORTS).getChanged());
	}

	@Test
	void testBuildConfigurationParam_enableTelemetryChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.ENABLE_TELEMETRY)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.ENABLE_TELEMETRY).getChanged());
	}

	@Test
	void testBuildConfigurationParam_manageBinariesChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.MANAGE_BINARIES_AUTOMATICALLY)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.MANAGE_BINARIES_AUTOMATICALLY).getChanged());
	}

	@Test
	void testBuildConfigurationParam_cliPathChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.CLI_PATH)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.CLI_PATH).getChanged());
	}

	@Test
	void testBuildConfigurationParam_cliBaseDownloadUrlChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.CLI_BASE_URL)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.CLI_BASE_DOWNLOAD_URL).getChanged());
	}

	@Test
	void testBuildConfigurationParam_authenticationMethodChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.AUTHENTICATION_METHOD)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.AUTHENTICATION_METHOD).getChanged());
	}

	@Test
	void testBuildConfigurationParam_enableDeltaChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.ENABLE_DELTA)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.ENABLE_DELTA_FINDINGS).getChanged());
	}

	@Test
	void testBuildConfigurationParam_riskScoreThresholdChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.RISK_SCORE_THRESHOLD)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.RISK_SCORE_THRESHOLD).getChanged());
	}

	@Test
	void testBuildConfigurationParam_issueViewOpenIssuesChangedFlag() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertTrue(param.getSettings().get(LsSettingsKeys.ISSUE_VIEW_OPEN_ISSUES).getChanged());
	}

	@Test
	void testBuildConfigurationParam_issueViewIgnoredIssuesUnchangedWhenNotExplicitlyChanged() {
		setupPreferenceMock();

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		assertFalse(param.getSettings().get(LsSettingsKeys.ISSUE_VIEW_IGNORED_ISSUES).getChanged());
	}

	@Test
	void testBuildConfigurationParam_severityFilterSentAsCompositeSetting() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.FILTER_SHOW_CRITICAL)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting setting = param.getSettings().get(LsSettingsKeys.ENABLED_SEVERITIES);
		assertNotNull(setting);
		assertTrue(setting.getChanged());

		@SuppressWarnings("unchecked")
		Map<String, Boolean> severityMap = (Map<String, Boolean>) setting.getValue();
		assertEquals(true, severityMap.get("critical"));
		assertEquals(false, severityMap.get("high"));
		assertEquals(true, severityMap.get("medium"));
		assertEquals(false, severityMap.get("low"));
	}

	@Test
	void testBuildConfigurationParam_severityFilterChangedWhenAnySeverityChanged() {
		setupPreferenceMock();
		when(preferenceMock.isExplicitlyChanged(Preferences.FILTER_SHOW_LOW)).thenReturn(true);

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting setting = param.getSettings().get(LsSettingsKeys.ENABLED_SEVERITIES);
		assertTrue(setting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_severityFilterUnchangedWhenNoSeverityChanged() {
		setupPreferenceMock();

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		ConfigSetting setting = param.getSettings().get(LsSettingsKeys.ENABLED_SEVERITIES);
		assertNotNull(setting);
		assertFalse(setting.getChanged());
	}

	@Test
	void testBuildConfigurationParam_noIndividualSeverityFilterKeys() {
		setupPreferenceMock();

		var param = new LsConfigurationUpdater().buildConfigurationParam();
		Map<String, ConfigSetting> settings = param.getSettings();
		assertFalse(settings.containsKey("severity_filter_critical"));
		assertFalse(settings.containsKey("severity_filter_high"));
		assertFalse(settings.containsKey("severity_filter_medium"));
		assertFalse(settings.containsKey("severity_filter_low"));
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
		assertTrue(settings.containsKey("enabled_severities"));
		assertTrue(settings.containsKey("issue_view_open_issues"));
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
		when(preferenceMock.getPref(Preferences.ENDPOINT_KEY, "")).thenReturn("endpoint");
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

		// Issue view options mocks
		when(preferenceMock.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, true)).thenReturn(true);
		when(preferenceMock.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, false)).thenReturn(false);
	}
}
