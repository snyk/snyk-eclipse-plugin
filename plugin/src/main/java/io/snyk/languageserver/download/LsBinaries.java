package io.snyk.languageserver.download;

import java.net.URI;

public class LsBinaries {
  private static final String LS_DOWNLOAD_BASE_URL = "https://static.snyk.io/cli";
  public static final String REQUIRED_LS_PROTOCOL_VERSION = "10";

  public static URI getBaseUri() {
    return URI.create(LS_DOWNLOAD_BASE_URL);
  }

  public static URI getAssetUri(String assetName, String version) {
    return URI.create(String.format("%s/%s/%s", LS_DOWNLOAD_BASE_URL, version, assetName));
  }
}
