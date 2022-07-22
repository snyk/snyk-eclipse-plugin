package io.snyk.languageserver;

import io.snyk.eclipse.plugin.properties.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

import java.io.File;
import java.io.IOException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SuppressWarnings("ResultOfMethodCallIgnored")
public class LsBaseTest {
  protected LsRuntimeEnvironment environment = null;

  private File lsFile = getTempFile();

  @BeforeEach
  protected void setUp() {
    if (lsFile.exists()) lsFile.delete();
    lsFile = getTempFile();
    environment = mock(LsRuntimeEnvironment.class);
    InMemoryPreferenceStore store = new InMemoryPreferenceStore();
    store.put(Preferences.LS_BINARY_KEY, lsFile.toString());
    PreferencesUtils.setPreferences(Preferences.getInstance(store));

    when(environment.getArch()).thenReturn("amd64");
    when(environment.getOs()).thenReturn("linux");
    when(environment.getDownloadBinaryName(any())).thenReturn("snyk-ls_testVersion_linux_amd64");
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
