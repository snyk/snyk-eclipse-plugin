package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.runtime.Platform;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigs;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import io.snyk.languageserver.download.LsBinaries;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfigsParam;

class LsConfigurationUpdaterTest {
	private Preferences preferenceMock;

	@Mock
	private FolderConfigsParam folderConfigsParamMock;

	FolderConfigs mockFolderConfigs;

	@BeforeEach
	protected void setUp() {
		preferenceMock = mock(Preferences.class);
		PreferencesUtils.setPreferences(preferenceMock);

		mockFolderConfigs = mock(FolderConfigs.class);
		FolderConfigs.setInstance(mockFolderConfigs);
	}

	@Test
	void testGetSettings() {
		setupPreferenceMock();

		try (MockedStatic<Platform> platformMockedStatic = Mockito.mockStatic(Platform.class)) {
			var bundleMock = mock(Bundle.class);
			when(bundleMock.getVersion()).thenReturn(new Version("1.2.3"));
			platformMockedStatic.when(() -> Platform.getBundle(Activator.PLUGIN_ID)).thenReturn(bundleMock);
			var settings = new LsConfigurationUpdater().getCurrentSettings();

			assertEquals("iac", settings.getActivateSnykIac());
			assertEquals("code", settings.getActivateSnykCodeSecurity());
			assertEquals("oss", settings.getActivateSnykOpenSource());
			assertEquals("true", settings.getInsecure());
			assertEquals("endpoint", settings.getEndpoint());
			assertEquals("addParams", settings.getAdditionalParams());
			assertEquals("a=b;c=d", settings.getAdditionalEnv());
			assertEquals("path", settings.getPath());
			assertEquals("true", settings.getSendErrorReports());
			assertEquals("organization", settings.getOrganization());
			assertEquals("true", settings.getEnableTelemetry());
			assertEquals("true", settings.getManageBinariesAutomatically());
			assertEquals("/usr/local/bin/snyk", settings.getCliPath());
			assertEquals("ECLIPSE", settings.getIntegrationName());
			assertEquals(Activator.PLUGIN_VERSION, settings.getIntegrationVersion());
			assertEquals("false", settings.getAutomaticAuthentication());
			assertEquals(SystemUtils.JAVA_RUNTIME_NAME, settings.getRuntimeName());
			assertEquals(SystemUtils.JAVA_RUNTIME_VERSION, settings.getRuntimeVersion());
			assertEquals(SystemUtils.OS_ARCH, settings.getOsArch());
			assertEquals(SystemUtils.OS_NAME, settings.getOsPlatform());

			// Additional assertions based on setupPreferenceMock
			assertEquals("false", settings.getActivateSnykCodeQuality());
			assertEquals("/usr/local/bin/snyk", settings.getCliPath());
			assertEquals("my-token", settings.getToken());
			assertEquals("automatic", settings.getScanningMode());
			assertEquals("oauth", settings.getAuthenticationMethod());
			assertEquals(LsBinaries.REQUIRED_LS_PROTOCOL_VERSION, settings.getRequiredProtocolVersion());
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
		when(preferenceMock.getPref(Preferences.CLI_PATH, "")).thenReturn("/path");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE_QUALITY, Boolean.FALSE.toString()))
				.thenReturn("false");
		when(preferenceMock.getPref(Preferences.CLI_PATH, "")).thenReturn("/usr/local/bin/snyk");
		when(preferenceMock.getPref(Preferences.AUTH_TOKEN_KEY, "")).thenReturn("my-token");
		when(preferenceMock.getPref(Preferences.AUTHENTICATION_METHOD, "oauth")).thenReturn("oauth");
		when(preferenceMock.getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC)).thenReturn(true);
		when(preferenceMock.getPref(Preferences.ENABLE_DELTA, Boolean.FALSE.toString())).thenReturn("true");

		List<FolderConfig> mockFolderConfigsParam = new ArrayList<>();
		when(mockFolderConfigs.getAll()).thenReturn(mockFolderConfigsParam);

	}
}
