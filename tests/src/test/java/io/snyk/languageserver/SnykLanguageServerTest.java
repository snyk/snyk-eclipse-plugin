package io.snyk.languageserver;

import io.snyk.eclipse.plugin.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;

class SnykLanguageServerTest {

  @Test
  void getInitializationOptions() {
    PreferencesUtils.setPreferences(Preferences.getInstance(new InMemoryPreferenceStore()));
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(LsConfigurationUpdater.Settings.class, output);
  }

  @Test
  void getInitializationOptionsContainsTrustedPaths() {
    InMemoryPreferenceStore store = new InMemoryPreferenceStore();
    String trustedPaths = "a" + File.pathSeparatorChar + "b/c";
    store.put(Preferences.TRUSTED_FOLDERS, trustedPaths);
    PreferencesUtils.setPreferences(Preferences.getInstance(store));
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(LsConfigurationUpdater.Settings.class, output);
    LsConfigurationUpdater.Settings settings = (LsConfigurationUpdater.Settings) output;
    assertEquals("a", settings.getTrustedFolders()[0]);
    assertEquals("b/c", settings.getTrustedFolders()[1]);
  }
  
  @Test
  void getInitializationOptionsDoesNotContainsTrustedPathsIfNoneKnown() {
    InMemoryPreferenceStore store = new InMemoryPreferenceStore();
    PreferencesUtils.setPreferences(Preferences.getInstance(store));
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(LsConfigurationUpdater.Settings.class, output);
    LsConfigurationUpdater.Settings settings = (LsConfigurationUpdater.Settings) output;
    assertNotNull(settings.getTrustedFolders());
    assertEquals(0, settings.getTrustedFolders().length);
  }
}
