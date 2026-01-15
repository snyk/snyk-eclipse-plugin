package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.protocolextension.messageObjects.Settings;

class SnykLanguageServerTest extends LsBaseTest {

  private static final String NON_EXISTENT_CLI_PATH = "/non/existent/path/snyk-cli";

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

  @Test
  void getCliPathOrThrow_throwsWhenBinaryDoesNotExist() {
    this.prefs.store(Preferences.CLI_PATH, NON_EXISTENT_CLI_PATH);

    IOException exception = assertThrows(IOException.class,
        () -> SnykLanguageServer.getCliPathOrThrow(this.prefs));

    assertTrue(exception.getMessage().contains("not found"), "Should mention binary not found");
    assertTrue(exception.getMessage().contains(NON_EXISTENT_CLI_PATH), "Should include the path");
    assertTrue(exception.getMessage().contains("Snyk Preferences"), "Should mention preferences");
  }

  @Test
  void getCliPathOrThrow_suggestsErrorLogWhenManagedBinariesEnabled() {
    this.prefs.store(Preferences.CLI_PATH, NON_EXISTENT_CLI_PATH);
    this.prefs.store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "true");

    IOException exception = assertThrows(IOException.class,
        () -> SnykLanguageServer.getCliPathOrThrow(this.prefs));

    assertTrue(exception.getMessage().contains("check the Error Log"), "Should suggest checking Error Log");
    assertTrue(exception.getMessage().contains("Manage Binaries Automatically"), "Should mention the setting name"); // as this error line is only shown when the setting is enabled
  }

  @Test
  void getCliPathOrThrow_returnsPathWhenBinaryExists() throws IOException {
    File existingBinary = File.createTempFile("snyk-cli-test", ".exe");
    existingBinary.deleteOnExit();
    this.prefs.store(Preferences.CLI_PATH, existingBinary.getAbsolutePath());

    String result = SnykLanguageServer.getCliPathOrThrow(this.prefs);

    assertEquals(existingBinary.getAbsolutePath(), result);
  }
}
