package io.snyk.languageserver;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.FileSystemUtil;
import org.eclipse.core.internal.net.ProxyData;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.eclipse.core.runtime.Platform;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.Version;

import java.io.File;
import java.util.HashMap;

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
    environment = new LsRuntimeEnvironment(preferenceMock);
  }

  @Test
  void testDownloadBinaryNameConstructions() {
    var actual = environment.getDownloadBinaryName("testVersion");
    String expected = "snyk-ls_testVersion_" + environment.getOs() + "_" + environment.getArch();
    if (expected.contains("windows"))
      expected += ".exe";
    assertEquals(expected, actual);
  }

  @Test
  void testGetLsFileShouldReturnPreferenceIfSet() {
    String expectedLsPath = "testPath";
    when(preferenceMock.getLsBinary()).thenReturn(expectedLsPath);

    File file = environment.getLSFile();

    assertEquals(expectedLsPath, file.getPath());
  }

  @Test
  void testGetLsFileShouldReturnDefaultIfPreferenceNotSet() {
    when(preferenceMock.getLsBinary()).thenReturn(null);

    File file = environment.getLSFile();

    assertEquals(new File(FileSystemUtil.getCliDirectory(), environment.getBinaryName()), file);
  }

  //    @Test
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
}
