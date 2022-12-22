package io.snyk.languageserver;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.runtime.Platform;

class LsConfigurationUpdaterTest {
  private Preferences preferenceMock;

  @BeforeEach
  protected void setUp() {
    preferenceMock = mock(Preferences.class);
    PreferencesUtils.setPreferences(preferenceMock);
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
      assertEquals("code", settings.getActivateSnykCode());
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
      assertEquals("/path", settings.getCliPath());
      assertEquals("ECLIPSE", settings.getIntegrationName());
      assertEquals(Activator.PLUGIN_VERSION, settings.getIntegrationVersion());
      assertEquals("false", settings.getAutomaticAuthentication());
      assertEquals(SystemUtils.JAVA_RUNTIME_NAME, settings.getRuntimeName());
      assertEquals(SystemUtils.JAVA_RUNTIME_VERSION, settings.getRuntimeVersion());
      assertEquals(SystemUtils.OS_ARCH, settings.getOsArch());
      assertEquals(SystemUtils.OS_NAME, settings.getOsPlatform());
    }
  }

  private void setupPreferenceMock() {
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_IAC, "true")).thenReturn("iac");
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_IAC, "false")).thenReturn("iac");
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_IAC, "")).thenReturn("iac");
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE, "true")).thenReturn("code");
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE, "false")).thenReturn("code");
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE, "")).thenReturn("code");
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
  }
}
