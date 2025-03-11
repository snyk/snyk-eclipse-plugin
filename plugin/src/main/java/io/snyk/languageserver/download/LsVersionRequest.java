package io.snyk.languageserver.download;

import org.apache.http.client.methods.HttpGet;

public class LsVersionRequest extends HttpGet {
  public LsVersionRequest(String releaseChannel) {
    setURI(LsBinaries.getAssetUri("ls-protocol-version-" + LsBinaries.REQUIRED_LS_PROTOCOL_VERSION, releaseChannel)); // NOPMD by bdoetsch on 3/11/25, 1:39 PM
  }
}
