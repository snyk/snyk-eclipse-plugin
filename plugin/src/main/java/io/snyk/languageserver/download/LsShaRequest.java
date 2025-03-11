package io.snyk.languageserver.download;

import org.apache.http.client.methods.HttpGet;

public class LsShaRequest extends HttpGet {
  private static final String SHA_FILENAME = "sha256sums.txt.asc";

  public LsShaRequest(String version) {
    String filename = String.format(SHA_FILENAME, version);
    setURI(LsBinaries.getAssetUri(filename, "v" + version)); // NOPMD by bdoetsch on 3/11/25, 1:39 PM
  }
}
