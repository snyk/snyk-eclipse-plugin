package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.jupiter.api.Test;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.LsKey;

class SnykLanguageServerTest extends LsBaseTest {

  private static final String NON_EXISTENT_CLI_PATH = "/non/existent/path/snyk-cli";

  @Test
  void getInitializationOptions() {
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(JsonElement.class, output);
    JsonObject root = ((JsonElement) output).getAsJsonObject();
    assertTrue(root.has("settings"), "root should have 'settings' key");
    assertTrue(root.has("folderConfigs"), "root should have 'folderConfigs' key");

    // Metadata fields should be at the top level
    assertTrue(root.has("integrationName"), "root should have 'integrationName'");
    assertTrue(root.has("requiredProtocolVersion"), "root should have 'requiredProtocolVersion'");

    JsonObject settings = root.getAsJsonObject("settings");
    for (var field : new String[] {
        LsKey.ENDPOINT.key, LsKey.ORGANIZATION.key, LsKey.TOKEN.key,
        LsKey.ACTIVATE_SNYK_CODE.key, LsKey.ACTIVATE_SNYK_OPEN_SOURCE.key,
        LsKey.ACTIVATE_SNYK_IAC.key, LsKey.INSECURE.key,
        LsKey.ADDITIONAL_PARAMS.key, LsKey.SCANNING_MODE.key,
        LsKey.CLI_PATH.key, LsKey.CLI_BASE_DOWNLOAD_URL.key,
        LsKey.AUTHENTICATION_METHOD.key,
        LsKey.MANAGE_BINARIES_AUTOMATICALLY.key,
        LsKey.SEVERITY_FILTER_CRITICAL.key,
        LsKey.SEVERITY_FILTER_HIGH.key,
        LsKey.SEVERITY_FILTER_MEDIUM.key,
        LsKey.SEVERITY_FILTER_LOW.key,
        LsKey.ISSUE_VIEW_OPEN_ISSUES.key, LsKey.ISSUE_VIEW_IGNORED_ISSUES.key
    }) {
      assertTrue(settings.has(field), "settings should contain '" + field + "'");
      assertTrue(settings.getAsJsonObject(field).has("value"),
          "'" + field + "' should have a 'value' property");
    }
  }

  @Test
  void getInitializationOptionsContainsTrustedPaths() {
    String trustedPaths = "a" + File.pathSeparatorChar + "b/c";
    this.prefs.store(Preferences.TRUSTED_FOLDERS, trustedPaths);
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(JsonElement.class, output);
    JsonObject root = ((JsonElement) output).getAsJsonObject();
    var foldersArray = root.getAsJsonArray("trustedFolders");
    assertNotNull(foldersArray);
    assertEquals(2, foldersArray.size());
    assertEquals("a", foldersArray.get(0).getAsString());
    assertEquals("b/c", foldersArray.get(1).getAsString());
  }

  @Test
  void getInitializationOptionsDoesNotContainsTrustedPathsIfNoneKnown() {
    SnykLanguageServer snykStreamConnectionProvider = new SnykLanguageServer();

    Object output = snykStreamConnectionProvider.getInitializationOptions(null);

    assertInstanceOf(JsonElement.class, output);
    JsonObject root = ((JsonElement) output).getAsJsonObject();
    var foldersArray = root.getAsJsonArray("trustedFolders");
    assertNotNull(foldersArray);
    assertEquals(0, foldersArray.size());
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
