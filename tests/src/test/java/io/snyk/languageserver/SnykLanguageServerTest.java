package io.snyk.languageserver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeFalse;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.File;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Set;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.LsKey;
import io.snyk.languageserver.download.HttpClientFactory;
import io.snyk.languageserver.download.LsBinaries;
import io.snyk.languageserver.download.LsDownloader;

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

  @Test
  void verifyCliProtocolVersion_passesWhenVersionMatches() throws Exception {
    assumeFalse(System.getProperty("os.name").toLowerCase().contains("win"),
        "Shell script not supported on Windows");
    File script = createCliStub(LsBinaries.REQUIRED_LS_PROTOCOL_VERSION);

    SnykLanguageServer.verifyCliProtocolVersion(script.getAbsolutePath());
  }

  @Test
  void verifyCliProtocolVersion_throwsWhenVersionMismatches() throws Exception {
    assumeFalse(System.getProperty("os.name").toLowerCase().contains("win"),
        "Shell script not supported on Windows");
    File script = createCliStub("999");

    IOException ex = assertThrows(IOException.class,
        () -> SnykLanguageServer.verifyCliProtocolVersion(script.getAbsolutePath()));

    assertTrue(ex.getMessage().contains("protocol version mismatch"));
    assertTrue(ex.getMessage().contains("999"));
    assertTrue(ex.getMessage().contains(LsBinaries.REQUIRED_LS_PROTOCOL_VERSION));
  }

  @Test
  void verifyCliProtocolVersion_throwsWithClearMessageOnUnparseableOutput() throws Exception {
    assumeFalse(System.getProperty("os.name").toLowerCase().contains("win"),
        "Shell script not supported on Windows");
    File script = createCliStub("not-a-number");

    IOException ex = assertThrows(IOException.class,
        () -> SnykLanguageServer.verifyCliProtocolVersion(script.getAbsolutePath()));

    assertTrue(ex.getMessage().contains("not compatible"), "Should say binary is not compatible");
    assertFalse(ex.getMessage().contains("NumberFormatException"), "Should not expose internal exception type");
  }

  @Test
  void verifyCliProtocolVersion_throwsWithTruncatedOutputNotFullHelpText() throws Exception {
    assumeFalse(System.getProperty("os.name").toLowerCase().contains("win"),
        "Shell script not supported on Windows");
    // Simulate an old CLI that outputs a very long message (90+ chars) instead of a version number.
    // Using a simple safe string to avoid shell escaping issues.
    String longOutput = "This is not a version number and it is deliberately very long output text here abcdefghijklmno";
    File script = createCliStub(longOutput);

    IOException ex = assertThrows(IOException.class,
        () -> SnykLanguageServer.verifyCliProtocolVersion(script.getAbsolutePath()));

    assertTrue(ex.getMessage().length() < 300, "Error message should be short, not dump full help text");
    assertTrue(ex.getMessage().contains("..."), "Long output should be truncated with ellipsis");
  }

  @Test
  void verifyCliProtocolVersion_parsesVersionFromFirstLineIgnoringTrailingOutput() throws Exception {
    assumeFalse(System.getProperty("os.name").toLowerCase().contains("win"),
        "Shell script not supported on Windows");
    // Simulate a CLI that prints version on first line then a warning on the second line.
    File script = File.createTempFile("snyk-cli-stub", ".sh");
    script.deleteOnExit();
    Files.writeString(script.toPath(),
        "#!/bin/sh\nprintf '" + LsBinaries.REQUIRED_LS_PROTOCOL_VERSION + "\\nWarning: something deprecated\\n'\n");
    Files.setPosixFilePermissions(script.toPath(),
        Set.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
            PosixFilePermission.OWNER_EXECUTE));

    // Should succeed — version is on the first line and matches.
    SnykLanguageServer.verifyCliProtocolVersion(script.getAbsolutePath());
  }

  @Test
  @Tag("smoke")
  // The real downloaded CLI supports --protocolVersion; verify the check passes against it.
  void verifyCliProtocolVersion_withRealCliDownload_passesWhenVersionSupported() throws Exception {
    assumeTrue(isNetworkAvailable(), "Network not available — skipping smoke test");

    File tempBinary = File.createTempFile("snyk-cli-smoke", "");
    tempBinary.deleteOnExit();
    String originalCliPath = prefs.getCliPath();
    try {
      prefs.store(Preferences.CLI_PATH, tempBinary.getAbsolutePath());
      LsRuntimeEnvironment realEnv = new LsRuntimeEnvironment();
      LsDownloader downloader = new LsDownloader(HttpClientFactory.getInstance(), realEnv, null);
      downloader.download(new NullProgressMonitor());
      // The downloaded CLI must support --protocolVersion and return the required version.
      SnykLanguageServer.verifyCliProtocolVersion(tempBinary.getAbsolutePath());
    } finally {
      prefs.store(Preferences.CLI_PATH, originalCliPath);
      tempBinary.delete();
    }
  }

  private static boolean isNetworkAvailable() {
    try (Socket socket = new Socket()) {
      socket.connect(new InetSocketAddress("downloads.snyk.io", 443), 3000);
      return true;
    } catch (IOException e) {
      return false;
    }
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
