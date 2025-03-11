package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;

import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.Preferences;

class SnykLanguageServerTest extends LsBaseTest {

  @Test
  void getInitializationOptions() {
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(LsConfigurationUpdater.Settings.class, output);
  }

  @Test
  void getInitializationOptionsContainsTrustedPaths() {
    String trustedPaths = "a" + File.pathSeparatorChar + "b/c";
    this.prefs.store(Preferences.TRUSTED_FOLDERS, trustedPaths);
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(LsConfigurationUpdater.Settings.class, output);
    LsConfigurationUpdater.Settings settings = (LsConfigurationUpdater.Settings) output;
    assertEquals("a", settings.getTrustedFolders()[0]);
    assertEquals("b/c", settings.getTrustedFolders()[1]);
  }

  @Test
  void getInitializationOptionsDoesNotContainsTrustedPathsIfNoneKnown() {
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(LsConfigurationUpdater.Settings.class, output);
    LsConfigurationUpdater.Settings settings = (LsConfigurationUpdater.Settings) output;
    assertNotNull(settings.getTrustedFolders());
    assertEquals(0, settings.getTrustedFolders().length);
  }
}
