package io.snyk.eclipse.plugin.runner;

import io.snyk.eclipse.plugin.properties.Preferences;
import org.eclipse.core.runtime.ILog;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ProcessRunnerTest {
  private Preferences preferenceMock;

  @BeforeEach
  void setUp() {
    preferenceMock = mock(Preferences.class);
  }

  @Test
  void testGetProcessBuilderLinux() {
    ILog logger = mock(ILog.class);
    Bundle bundle = mock(Bundle.class);
    when(preferenceMock.getPref(Preferences.INSECURE_KEY)).thenReturn("true");
    when(preferenceMock.getEndpoint()).thenReturn("endpoint");
    when(preferenceMock.getPref(Preferences.ORGANIZATION_KEY)).thenReturn("organization");
    when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY)).thenReturn("true");
    when(bundle.getVersion()).thenReturn(new Version(2, 0, 0));

    ProcessRunner cut = new ProcessRunner(preferenceMock, bundle, logger);
    ProcessBuilder builder = cut.createLinuxProcessBuilder(List.of("test"), Optional.of("good:path"));

    var env = builder.environment();
    var cmd = builder.command();
    assertTrue(cmd.contains("--insecure"));
    assertTrue(cmd.contains("--org=organization"));
    assertEquals("endpoint", env.get("SNYK_API"));
    assertTrue(env.get("PATH").contains("good:path"));
    assertEquals("organization", env.get(Preferences.ORGANIZATION_KEY));
    assertEquals("0", env.get(Preferences.ENABLE_TELEMETRY));
    verify(preferenceMock).getEndpoint();
    verify(preferenceMock).getPref(Preferences.INSECURE_KEY);
    verify(preferenceMock).getPref(Preferences.ORGANIZATION_KEY);
    verify(preferenceMock).getPref(Preferences.ENABLE_TELEMETRY);
  }

  @Test
  void testGetProcessBuilderLinuxSecure() {
    ILog logger = mock(ILog.class);
    Bundle bundle = mock(Bundle.class);
    when(preferenceMock.getPref(Preferences.INSECURE_KEY)).thenReturn("false");
    when(preferenceMock.getEndpoint()).thenReturn("endpoint");
    when(preferenceMock.getPref(Preferences.ORGANIZATION_KEY)).thenReturn("organization");
    when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY)).thenReturn("false");
    when(bundle.getVersion()).thenReturn(new Version(2, 0, 0));

    ProcessRunner cut = new ProcessRunner(preferenceMock, bundle, logger);
    ProcessBuilder builder = cut.createLinuxProcessBuilder(List.of("test"), Optional.of("good:path"));

    var env = builder.environment();
    var cmd = builder.command();
    assertFalse(cmd.contains("--insecure"));
  }

  @Test
  void testGetProcessBuilderLinuxNoOrg() {
    ILog logger = mock(ILog.class);
    Bundle bundle = mock(Bundle.class);
    when(preferenceMock.getPref(Preferences.INSECURE_KEY)).thenReturn("true");
    when(preferenceMock.getEndpoint()).thenReturn("endpoint");
    when(preferenceMock.getPref(Preferences.ORGANIZATION_KEY)).thenReturn("");
    when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY)).thenReturn("false");
    when(bundle.getVersion()).thenReturn(new Version(2, 0, 0));

    ProcessRunner cut = new ProcessRunner(preferenceMock, bundle, logger);
    ProcessBuilder builder = cut.createLinuxProcessBuilder(List.of("test"), Optional.of("good:path"));

    var env = builder.environment();
    var cmd = builder.command();
    assertFalse(cmd.contains("--org="));
    assertEquals(null, env.get(Preferences.ORGANIZATION_KEY));
  }
}
