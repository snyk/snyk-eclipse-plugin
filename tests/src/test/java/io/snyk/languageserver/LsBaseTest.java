package io.snyk.languageserver;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearAllCaches;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.net.proxy.IProxyService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

import io.snyk.eclipse.plugin.properties.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;

@SuppressWarnings("ResultOfMethodCallIgnored")
public class LsBaseTest {
  protected LsRuntimeEnvironment environment = null;
  protected IProxyService proxyServiceMock;

  private File lsFile = getTempFile();

  @BeforeEach
  protected void setUp() {
    clearAllCaches();
    if (lsFile.exists())
      lsFile.delete();
    lsFile = getTempFile();
    environment = mock(LsRuntimeEnvironment.class);
    InMemoryPreferenceStore store = new InMemoryPreferenceStore();
    PreferencesUtils.setPreferences(Preferences.getInstance(store));

    when(environment.getArch()).thenReturn("amd64");
    when(environment.getOs()).thenReturn("linux");
    when(environment.getDownloadBinaryName()).thenReturn("snyk-ls_testVersion_linux_amd64");
    proxyServiceMock = mock(IProxyService.class);
    when(environment.getProxyService()).thenReturn(proxyServiceMock);
  }

  @AfterEach
  void tearDown() {
    lsFile.delete();
    clearAllCaches();
  }

  protected File getTempFile() {
    try {
      File tempFile = File.createTempFile("ls-test", "tmp");
      tempFile.deleteOnExit();
      return tempFile;
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
