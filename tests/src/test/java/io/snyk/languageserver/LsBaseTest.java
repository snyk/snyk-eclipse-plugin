package io.snyk.languageserver;

import static org.mockito.ArgumentMatchers.any;
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
  protected LsRuntimeEnvironment environmentMock = null;
  protected IProxyService proxyServiceMock;

  private File lsFile = getTempFile();

  @BeforeEach
  protected void setUp() {
    if (lsFile.exists())
      lsFile.delete();
    lsFile = getTempFile();
    environmentMock = mock(LsRuntimeEnvironment.class);
    InMemoryPreferenceStore store = new InMemoryPreferenceStore();
    store.put(Preferences.LS_BINARY_KEY, lsFile.toString());
    PreferencesUtils.setPreferences(Preferences.getInstance(store));

    when(environmentMock.getArch()).thenReturn("amd64");
    when(environmentMock.getOs()).thenReturn("linux");
    when(environmentMock.getDownloadBinaryName(any())).thenReturn("snyk-ls_testVersion_linux_amd64");
    proxyServiceMock = mock(IProxyService.class);
    when(environmentMock.getProxyService()).thenReturn(proxyServiceMock);
  }

  @AfterEach
  void tearDown() {
    lsFile.delete();
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
