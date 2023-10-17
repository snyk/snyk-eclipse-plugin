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
    String content = "10";
    InputStream is;
    try {
      is = httpResponse.getEntity().getContent();
      content = new String(is.readAllBytes(), Charset.defaultCharset());
    } catch (UnsupportedOperationException | IOException e) {
      throw new RuntimeException(e);
    }
    return content;
  }
}
