package io.snyk.languageserver.download;

import io.snyk.languageserver.LsRuntimeEnvironment;
import org.apache.http.client.methods.HttpGet;

import java.net.URISyntaxException;


public class LsShaRequest extends HttpGet {
  private static final String SHA_FILENAME = "snyk-ls_%s_SHA256SUMS";

  public LsShaRequest(String version) {
    String filename = String.format(SHA_FILENAME, version);
    setURI(LsBinaries.getAssetUri(filename));
  }
}
