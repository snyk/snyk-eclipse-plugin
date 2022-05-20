package io.snyk.eclipse.plugin;

import org.eclipse.core.runtime.ILog;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

class SnykStartupTest {
  @Test
  void testIsDownloadAllowed() {
    SnykStartup cut = new SnykStartup();
    cut.setLogger(mock(ILog.class));
    assertTrue(cut.isDownloadAllowed(""));
    assertTrue(cut.isDownloadAllowed(null));
    assertFalse(cut.isDownloadAllowed("a"));
  }
}
