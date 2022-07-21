package io.snyk.languageserver.download;

import java.net.URI;

public class LsBinaries {
  private static final String LS_DOWNLOAD_BASE_URL = "https://static.snyk.io/snyk-ls";
  public static final String REQUIRED_LSP_VERSION = "2";

  public static URI getBaseUri() {
    return URI.create(String.format("%s/%s", LS_DOWNLOAD_BASE_URL, REQUIRED_LSP_VERSION));
  }

  public static URI getAssetUri(String assetName) {
    return URI.create(String.format("%s/%s/%s", LS_DOWNLOAD_BASE_URL, REQUIRED_LSP_VERSION, assetName));
  }
}
