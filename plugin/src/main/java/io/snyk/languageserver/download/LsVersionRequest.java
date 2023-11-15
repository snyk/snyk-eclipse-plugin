package io.snyk.languageserver.download;

import org.apache.http.client.methods.HttpGet;

import java.net.URISyntaxException;


public class LsVersionRequest extends HttpGet {
  public LsVersionRequest() {
    setURI(LsBinaries.getAssetUri("metadata.json"));
  }
}
