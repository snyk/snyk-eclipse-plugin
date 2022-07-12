package io.snyk.eclipse.plugin;

import org.eclipse.core.runtime.ILog;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.properties.Preferences;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class SnykStartupTest {
  @Test
  void testIsDownloadAllowed() {
    SnykStartup cut = new SnykStartup();
    cut.setLogger(mock(ILog.class));
    Preferences p = new Preferences();
    p.store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "true");

    
    p.store(Preferences.LS_BINARY_KEY, "");
    assertTrue(cut.isDownloadAllowed(p));
    
    p.store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "true");
    assertFalse(cut.isDownloadAllowed(p));

    p.store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "true");
    p.store(Preferences.LS_BINARY_KEY, null);
    assertTrue(cut.isDownloadAllowed(p));
    
    p.store(Preferences.LS_BINARY_KEY, "a");
    assertFalse(cut.isDownloadAllowed(p));
  }
}
