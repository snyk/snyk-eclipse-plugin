package io.snyk.eclipse.plugin;

import org.eclipse.core.runtime.ILog;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class SnykStartupTest {
  @Test
  void testDownloadAllowed() {
    SnykStartup cut = new SnykStartup();
    cut.setLogger(mock(ILog.class));
    assertFalse(cut.downloadAllowed(""));
    assertFalse(cut.downloadAllowed(null));
    assertTrue(cut.downloadAllowed("a"));
  }
}
