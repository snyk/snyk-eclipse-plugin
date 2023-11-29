package io.snyk.languageserver.download;

import org.apache.http.client.methods.HttpGet;

import io.snyk.languageserver.LsRuntimeEnvironment;


public class LsDownloadRequest extends HttpGet {

  public LsDownloadRequest(String version, LsRuntimeEnvironment utils) {
    if (!version.startsWith("v") && !version.equals("latest")) version = "v" + version;
    setURI(LsBinaries.getAssetUri(utils.getDownloadBinaryName(), version));
  }
}
