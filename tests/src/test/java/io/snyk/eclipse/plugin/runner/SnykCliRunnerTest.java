package io.snyk.eclipse.plugin.runner;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;

import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.properties.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;

class SnykCliRunnerTest {

  @Test
  void testRunDoesntAllowScansOfUntrustedPath() {
    PreferencesUtils.setPreferences(Preferences.getInstance(new InMemoryPreferenceStore()));
    SnykCliRunner cut = new SnykCliRunner();
    File navigatePath = new File("untrusted/path");
    
    ProcessResult result = cut.snykTest(navigatePath);
    
    assertTrue(result.getError().endsWith(navigatePath.getAbsolutePath() + " is not trusted."));
  }

  @Test
  void testRunAllowsScanOfTrustedPath() {
    File navigatePath = new File("trusted/path");
    InMemoryPreferenceStore store = new InMemoryPreferenceStore();
    store.put(Preferences.TRUSTED_FOLDERS, "a" + File.pathSeparator + navigatePath.getAbsolutePath() + File.pathSeparator + "b");
    PreferencesUtils.setPreferences(Preferences.getInstance(store));

    SnykCliRunner cut = new SnykCliRunner();
    ProcessResult result = cut.snykTest(navigatePath);
    
    assertFalse(result.getError().endsWith(navigatePath.getAbsolutePath() + " is not trusted."));
  }
}
