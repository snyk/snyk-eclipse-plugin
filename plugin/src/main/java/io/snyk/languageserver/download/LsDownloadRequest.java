package io.snyk.languageserver.download;

import io.snyk.languageserver.LsRuntimeEnvironment;
import org.apache.http.client.methods.HttpGet;

import java.net.URISyntaxException;


public class LsDownloadRequest extends HttpGet {

  public LsDownloadRequest(String version, LsRuntimeEnvironment utils) {
    setURI(LsBinaries.getAssetUri(utils.getDownloadBinaryName(version)));
  }
}
