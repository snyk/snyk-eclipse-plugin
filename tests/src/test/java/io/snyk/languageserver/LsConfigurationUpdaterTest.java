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
		assertTrue(settings.containsKey(LsSettingsKeys.INSECURE));
		assertTrue(settings.containsKey(LsSettingsKeys.ADDITIONAL_PARAMS));
		assertTrue(settings.containsKey(LsSettingsKeys.PATH));
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
					"folder_path": "/test/project",
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
					"folder_path": "/workspace/project-a",
					"settings": {
						"base_branch": {"value": "develop", "changed": false, "source": "cli", "origin_scope": "folder"}
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

		// Serialize to verify it's valid JSON structure
		String json = gson.toJson(param);
		LspConfigurationParam deserialized = gson.fromJson(json, LspConfigurationParam.class);
		assertNotNull(deserialized.getSettings());
		assertNotNull(deserialized.getFolderConfigs());
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

		// Severity filter mocks
		when(preferenceMock.getBooleanPref(Preferences.FILTER_SHOW_CRITICAL, true)).thenReturn(true);
		when(preferenceMock.getBooleanPref(Preferences.FILTER_SHOW_HIGH, true)).thenReturn(false);
		when(preferenceMock.getBooleanPref(Preferences.FILTER_SHOW_MEDIUM, true)).thenReturn(true);
		when(preferenceMock.getBooleanPref(Preferences.FILTER_SHOW_LOW, true)).thenReturn(false);
	}
}
