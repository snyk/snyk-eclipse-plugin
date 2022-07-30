package io.snyk.languageserver;

import io.snyk.eclipse.plugin.properties.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SnykLanguageServerTest {

  @Test
  void getInitializationOptions() {
    PreferencesUtils.setPreferences(Preferences.getInstance(new InMemoryPreferenceStore()));
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(LsConfigurationUpdater.Settings.class, output);
  }
}
