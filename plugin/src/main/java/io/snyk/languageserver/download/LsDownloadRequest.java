package io.snyk.languageserver.download;

import org.apache.http.client.methods.HttpGet;

import io.snyk.languageserver.LsRuntimeEnvironment;


public class LsDownloadRequest extends HttpGet {

  public LsDownloadRequest(String version, LsRuntimeEnvironment utils) {
	var usedVersion = version;
    if (version != null && !version.startsWith("v") && !version.equals("latest")) usedVersion = "v" + version; // NOPMD by bdoetsch on 3/11/25, 1:39 PM
    setURI(LsBinaries.getAssetUri(utils.getDownloadBinaryName(), usedVersion)); // NOPMD by bdoetsch on 3/11/25, 10:59 AM
  }
}
