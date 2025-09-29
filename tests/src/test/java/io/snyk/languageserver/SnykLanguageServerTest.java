package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;

import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.protocolextension.messageObjects.Settings;

class SnykLanguageServerTest extends LsBaseTest {

  @Test
  void getInitializationOptions() {
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(Settings.class, output);
  }

  @Test
  void getInitializationOptionsContainsTrustedPaths() {
    String trustedPaths = "a" + File.pathSeparatorChar + "b/c";
    this.prefs.store(Preferences.TRUSTED_FOLDERS, trustedPaths);
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(Settings.class, output);
    Settings settings = (Settings) output;
    assertEquals("a", settings.getTrustedFolders()[0]);
    assertEquals("b/c", settings.getTrustedFolders()[1]);
  }

  @Test
  void getInitializationOptionsDoesNotContainsTrustedPathsIfNoneKnown() {
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(Settings.class, output);
    Settings settings = (Settings) output;
    assertNotNull(settings.getTrustedFolders());
    assertEquals(0, settings.getTrustedFolders().length);
  }
}
