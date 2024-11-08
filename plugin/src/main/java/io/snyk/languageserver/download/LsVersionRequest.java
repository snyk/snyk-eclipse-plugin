package io.snyk.languageserver.download;

import org.apache.http.client.methods.HttpGet;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class LsVersionRequest extends HttpGet {
  public LsVersionRequest() {
    setURI(LsBinaries.getAssetUri("ls-protocol-version-" + LsBinaries.REQUIRED_LS_PROTOCOL_VERSION, Preferences.getInstance().getReleaseChannel()));
  }
}
