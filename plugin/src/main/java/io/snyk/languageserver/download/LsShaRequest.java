package io.snyk.languageserver.download;

import io.snyk.languageserver.LsRuntimeEnvironment;
import org.apache.http.client.methods.HttpGet;

import java.net.URI;
import java.net.URISyntaxException;


public class LsShaRequest extends HttpGet {
  private static final String DOWNLOAD_URL = "https://static.snyk.io/snyk-ls/snyk-ls_%s_SHA256SUMS";

  public LsShaRequest(String version, LsRuntimeEnvironment utils) {
    try {
      setURI(new URI(String.format(DOWNLOAD_URL, version)));
    } catch (URISyntaxException e) {
      throw new RuntimeException(e);
    }
  }
}
