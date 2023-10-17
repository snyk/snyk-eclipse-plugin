package io.snyk.eclipse.plugin.properties.preferences;

import io.snyk.eclipse.plugin.EnvironmentConstants;
import org.apache.commons.lang3.SystemUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

@SuppressWarnings("restriction")
class PreferencesTest {

  @BeforeEach
  void setUp() {
    PreferencesUtils.setPreferences(null);
  }

  @Test
  void test_DefaultPreferences() {
    Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

    assertEquals("false", prefs.getPref(Preferences.ACTIVATE_SNYK_CODE));
    assertEquals("true", prefs.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
    assertEquals("true", prefs.getPref(Preferences.ACTIVATE_SNYK_IAC));
    assertEquals("true", prefs.getPref(Preferences.SEND_ERROR_REPORTS));
    assertEquals("true", prefs.getPref(Preferences.ENABLE_TELEMETRY));
    assertEquals("true", prefs.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY));
    assertEquals("true", prefs.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY));
    assertEquals("1", prefs.getPref(Preferences.LSP_VERSION));
    assertEquals(Preferences.AUTH_METHOD_TOKEN, prefs.getPref(Preferences.AUTHENTICATION_METHOD));
  }

  @Test
  void test_ExistingTokenInEnvironment_IsStoredInPreferences() {
    try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
      mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_TOKEN, "")).thenReturn("token");

      Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

      assertEquals(prefs.getAuthToken(), "token");
    }
  }

  @Test
  void test_ExistingEndpointInEnvironment_IsStoredInPreferences() {
    try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
      mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_API, "")).thenReturn("https://custom.endpoint.io");

      Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

      assertEquals(prefs.getEndpoint(), "https://custom.endpoint.io");
    }
  }

  @Test
  void test_ExistingOrgInEnvironment_IsStoredInPreferences() {
    try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
      mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_ORG, "")).thenReturn("myOrg");

      Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

      assertEquals(prefs.getPref(Preferences.ORGANIZATION_KEY), "myOrg");
    }
  }

  @Test
  void test_GetBoolean_returnsBooleanProperty() {
    Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

    assertFalse(prefs.getBooleanPref(Preferences.ACTIVATE_SNYK_CODE));
    assertTrue(prefs.getBooleanPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
  }

  @Test
  void test_GetBoolean_returnsFalseForNonBooleanProperty() {
    Preferences prefs = Preferences.getInstance(new InMemoryPreferenceStore());

    assertFalse(prefs.getBooleanPref(Preferences.CLI_PATH));
  }
}
