package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Set;

import org.junit.jupiter.api.Test;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.download.LsBinaries;

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
        LsSettingsKeys.ENDPOINT, LsSettingsKeys.ORGANIZATION, LsSettingsKeys.TOKEN,
        LsSettingsKeys.ACTIVATE_SNYK_CODE, LsSettingsKeys.ACTIVATE_SNYK_OPEN_SOURCE,
        LsSettingsKeys.ACTIVATE_SNYK_IAC, LsSettingsKeys.INSECURE,
        LsSettingsKeys.ADDITIONAL_PARAMS, LsSettingsKeys.SCANNING_MODE,
        LsSettingsKeys.CLI_PATH, LsSettingsKeys.CLI_BASE_DOWNLOAD_URL,
        LsSettingsKeys.AUTHENTICATION_METHOD,
        LsSettingsKeys.MANAGE_BINARIES_AUTOMATICALLY,
        LsSettingsKeys.ENABLED_SEVERITIES,
        LsSettingsKeys.ISSUE_VIEW_OPEN_ISSUES, LsSettingsKeys.ISSUE_VIEW_IGNORED_ISSUES
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

  @Test
  void verifyCliProtocolVersion_passesWhenVersionMatches() throws Exception {
    assumeFalse(System.getProperty("os.name").toLowerCase().contains("win"),
        "Shell script not supported on Windows");
    File script = createCliStub(LsBinaries.REQUIRED_LS_PROTOCOL_VERSION);

    // should not throw
    SnykLanguageServer.verifyCliProtocolVersion(script.getAbsolutePath());
  }

  @Test
  void verifyCliProtocolVersion_throwsWhenVersionMismatches() throws Exception {
    assumeFalse(System.getProperty("os.name").toLowerCase().contains("win"),
        "Shell script not supported on Windows");
    File script = createCliStub("999");

    IOException ex = assertThrows(IOException.class,
        () -> SnykLanguageServer.verifyCliProtocolVersion(script.getAbsolutePath()));

    assertTrue(ex.getMessage().contains("protocol version mismatch"), "Should mention version mismatch");
    assertTrue(ex.getMessage().contains("999"), "Should include actual version");
    assertTrue(ex.getMessage().contains(LsBinaries.REQUIRED_LS_PROTOCOL_VERSION), "Should include expected version");
  }

  @Test
  void verifyCliProtocolVersion_doesNotThrowOnUnparseableOutput() throws Exception {
    assumeFalse(System.getProperty("os.name").toLowerCase().contains("win"),
        "Shell script not supported on Windows");
    File script = createCliStub("not-a-number");

    // non-fatal: should not throw when output can't be parsed
    SnykLanguageServer.verifyCliProtocolVersion(script.getAbsolutePath());
  }

  private File createCliStub(String outputVersion) throws IOException {
    File script = File.createTempFile("snyk-cli-stub", ".sh");
    script.deleteOnExit();
    Files.writeString(script.toPath(), "#!/bin/sh\necho " + outputVersion + "\n");
    Files.setPosixFilePermissions(script.toPath(),
        Set.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
            PosixFilePermission.OWNER_EXECUTE));
    return script;
  }
}
