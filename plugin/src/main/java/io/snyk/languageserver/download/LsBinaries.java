package io.snyk.languageserver.download;

import static io.snyk.eclipse.plugin.properties.preferences.Preferences.CLI_BASE_URL;

import java.net.URI;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class LsBinaries {
  private static final Preferences PREFERENCES = Preferences.getInstance();
  public static final String REQUIRED_LS_PROTOCOL_VERSION = "14";

  public static URI getBaseUri() {
    return URI.create(PREFERENCES.getPref(CLI_BASE_URL));
  }

  public static URI getAssetUri(String assetName, String version) {
    return URI.create(String.format("%s/cli/%s/%s", PREFERENCES.getPref(CLI_BASE_URL), version, assetName));
  }
}
