package io.snyk.eclipse.plugin;

import org.eclipse.core.runtime.ILog;
import org.eclipse.equinox.internal.security.storage.SecurePreferencesWrapper;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.properties.Preferences;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class SnykStartupTest {
  @Test
  void testIsDownloadAllowed() {
    SnykStartup startup = new SnykStartup();
    startup.setLogger(mock(ILog.class));
    assertTrue(startup.isDownloadAllowed("", true));
    assertTrue(startup.isDownloadAllowed(null, true));
    assertFalse(startup.isDownloadAllowed("a", true));
    assertFalse(startup.isDownloadAllowed("", false));
  }
}
