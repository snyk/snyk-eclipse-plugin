package io.snyk.eclipse.plugin.utils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;

import org.apache.http.HttpResponse;
import org.apache.http.client.ResponseHandler;

public class LsMetadataResponseHandler implements ResponseHandler<String> {

  public LsMetadataResponseHandler() {
  }

  @Override
  public String handleResponse(HttpResponse httpResponse) {
    String latestCliSupportingLSProtocolVersion = "10";
    InputStream inputStream;
    try {
      inputStream = httpResponse.getEntity().getContent();
      latestCliSupportingLSProtocolVersion = new String(inputStream.readAllBytes(), Charset.defaultCharset());
    } catch (UnsupportedOperationException | IOException e) {
      throw new RuntimeException(e);
    }
    return latestCliSupportingLSProtocolVersion;
  }
}
