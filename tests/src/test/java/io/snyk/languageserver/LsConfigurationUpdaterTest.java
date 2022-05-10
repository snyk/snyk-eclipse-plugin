package io.snyk.languageserver;

import io.snyk.eclipse.plugin.properties.Preferences;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class LsConfigurationUpdaterTest {
  private Preferences preferenceMock;

  @BeforeEach
  protected void setUp() {
    preferenceMock = mock(Preferences.class);
  }

  @Test
  void testGetSettings() {
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_IAC, "true")).thenReturn("iac");
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE, "false")).thenReturn("code");
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "true")).thenReturn("oss");
    when(preferenceMock.getPref(Preferences.INSECURE_KEY, "false")).thenReturn("true");
    when(preferenceMock.getPref(Preferences.ENDPOINT_KEY, "")).thenReturn("endpoint");
    when(preferenceMock.getPref(Preferences.ADDITIONAL_PARAMETERS, "")).thenReturn("addParams");
    when(preferenceMock.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "")).thenReturn("a=b;c=d");
    when(preferenceMock.getPref(Preferences.PATH_KEY, "")).thenReturn("path");

    var settings = new LsConfigurationUpdater().getCurrentSettings(preferenceMock);

    assertEquals("iac", settings.getActivateSnykIac());
    assertEquals("code", settings.getActivateSnykCode());
    assertEquals("oss", settings.getActivateSnykOpenSource());
    assertEquals("true", settings.getInsecure());
    assertEquals("endpoint", settings.getEndpoint());
    assertEquals("addParams", settings.getAdditionalParams());
    assertEquals("a=b;c=d", settings.getAdditionalEnv());
    assertEquals("path", settings.getPath());
  }
}
