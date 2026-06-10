package io.snyk.languageserver.download;

import static io.snyk.eclipse.plugin.preferences.Preferences.CLI_BASE_URL;

import java.net.URI;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class LsBinaries {
  private static final Preferences PREFERENCES = Preferences.getInstance();
  public static final String REQUIRED_LS_PROTOCOL_VERSION = "25";
  // Canonical CDN host. Used when the CLI_BASE_URL preference is unset or blank
  // — otherwise the formatted URL has no host and HttpClient throws
  // "Target host is not specified".
  public static final String DEFAULT_CLI_BASE_URL = "https://downloads.snyk.io";

  public static URI getBaseUri() {
    return URI.create(resolveBaseUrl());
  }

  public static URI getAssetUri(String assetName, String version) {
    return URI.create(String.format("%s/cli/%s/%s", resolveBaseUrl(), version, assetName));
  }

  private static String resolveBaseUrl() {
    String configured = PREFERENCES.getPref(CLI_BASE_URL, DEFAULT_CLI_BASE_URL);
    if (configured == null || configured.isBlank()) {
      return DEFAULT_CLI_BASE_URL;
    }
    return configured;
  }
}
