package io.snyk.languageserver;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import org.eclipse.core.internal.net.ProxyData;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.security.storage.StorageException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.Version;

import java.util.HashMap;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class LsRuntimeEnvironmentTest extends LsBaseTest {
  private Preferences preferenceMock = null;

  @Override
  @BeforeEach
  protected void setUp() {
    preferenceMock = mock(Preferences.class);
    PreferencesUtils.setPreferences(preferenceMock);
    environment = new LsRuntimeEnvironment();
  }

  @Test
  void testDownloadBinaryNameConstructions() {
    var actual = environment.getDownloadBinaryName();
    String expected = "snyk-"+environment.getOs() + environment.getArch();
    if (expected.contains("win"))
      expected += ".exe";
    assertEquals(expected, actual);
  }

  //  @Test
  void testUpdateEnvironment() {
    try (MockedStatic<Platform> platformMockedStatic = Mockito.mockStatic(Platform.class)) {
      HashMap<String, String> env = new HashMap<>();
      Version version = new Version(42, 42, 42);
      Bundle bundleMock = mock(Bundle.class);
      BundleContext ctxMock = mock(BundleContext.class);
      IProxyService proxyServiceMock = mock(IProxyService.class);
      IProxyData[] proxyData = new IProxyData[]{
        new ProxyData("https", "http://localhost", 3128, false, "")
      };
      platformMockedStatic.when(() -> Platform.getBundle(Activator.PLUGIN_ID)).thenReturn(bundleMock);
      when(bundleMock.getBundleContext()).thenReturn(ctxMock);
      when(bundleMock.getVersion()).thenReturn(version);
      when(ctxMock.getServiceReference(IProxyService.class)).thenReturn(mock(ServiceReference.class));
      when(ctxMock.getService(any())).thenReturn(proxyServiceMock);
      when(proxyServiceMock.getProxyData()).thenReturn(proxyData);

      environment.updateEnvironment(env);

      assertEquals("http://localhost:3128", env.get("https_proxy"));
      assertEquals("ECLIPSE", env.get("SNYK_INTEGRATION_NAME"));
      assertEquals(version.toString(), env.get("SNYK_INTEGRATION_VERSION"));
    }
    assertThrows(NullPointerException.class, () -> Platform.getBundle(Activator.PLUGIN_ID).getVersion());
  }

  @Test
  void testAddProductEnablementEnablesDisablesProducts() throws StorageException {
    HashMap<String, String> env = new HashMap<>();

    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_IAC)).thenReturn("iac");
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE)).thenReturn("code");
    when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE)).thenReturn("oss");

    environment.addProductEnablement(env);

    assertEquals("oss", env.get(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
    assertEquals("iac", env.get(Preferences.ACTIVATE_SNYK_IAC));
    assertEquals("code", env.get(Preferences.ACTIVATE_SNYK_CODE));
  }

  @Test
  void testAddAdditionalParamsAndEnvAddsThemToEnvironment() throws StorageException {
    HashMap<String, String> env = new HashMap<>();
    when(preferenceMock.getPref(Preferences.ADDITIONAL_PARAMETERS, "")).thenReturn("addParams");
    when(preferenceMock.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "")).thenReturn("a=b;c=d;e=f=g");

    environment.addAdditionalParamsAndEnv(env);

    assertEquals("addParams", env.get(Preferences.ADDITIONAL_PARAMETERS));
    assertEquals("b", env.get("a"));
    assertEquals("d", env.get("c"));
    assertEquals("f=g", env.get("e"));
  }

  @Test
  void testSendErrorReportsIsAddedToEnvironment() throws StorageException {
    HashMap<String, String> env = new HashMap<>();
    when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "")).thenReturn("true");
    when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "true")).thenReturn("true");
    when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "false")).thenReturn("false");

    environment.addTelemetry(env);

    assertEquals("true", env.get(Preferences.SEND_ERROR_REPORTS));
  }

  @Test
  void testOrganizationIsAddedToEnvironment() throws StorageException {
    HashMap<String, String> env = new HashMap<>();
    when(preferenceMock.getPref(Preferences.ORGANIZATION_KEY, "")).thenReturn("org");

    environment.addOrganization(env);

    assertEquals("org", env.get(Preferences.ORGANIZATION_KEY));
  }

  @Test
  void testEnableTelemetryIsAddedToEnvironment() throws StorageException {
    HashMap<String, String> env = new HashMap<>();
    when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "")).thenReturn("true");
    when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "true")).thenReturn("true");
    when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "false")).thenReturn("true");

    environment.addTelemetry(env);
    // This is a bit confusing - CLI takes DISABLE as env variable, but we ask for ENABLE, so it's reverted
    assertEquals("0", env.get(Preferences.ENABLE_TELEMETRY));
  }
  
  @Test
  void testEnableTelemetryIsAddedToEnvironmentDisabled() throws StorageException {
    HashMap<String, String> env = new HashMap<>();
    when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "")).thenReturn("false");
    when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "true")).thenReturn("false");
    when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "false")).thenReturn("false");

    environment.addTelemetry(env);
    // This is a bit confusing - CLI takes DISABLE as env variable, but we ask for ENABLE, so it's reverted
    assertEquals("1", env.get(Preferences.ENABLE_TELEMETRY));
  }
  
  @Test
  void testAddPath() throws StorageException {
	String expected = "C;/myPath/:";
    HashMap<String, String> env = new HashMap<>();
    when(preferenceMock.getPath()).thenReturn(Optional.of(expected));
    
    environment.addPath(env);

    assert(env.get("PATH").startsWith(expected));
  }
}
