package io.snyk.eclipse.plugin.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

import io.snyk.eclipse.plugin.runner.ProcessResult;

public class MockHandler {

  // set to fake mock the CLI calls
  public static final boolean MOCK = false;

  private MockHandler() {
  }

  public static ProcessResult getMockContent(String result) {
    return new ProcessResult(result, null);
  }

  public static ProcessResult getMockScanResult() {
    return getMockContent(getResource());
  }

  private static String getResource() {

    try {
      URL url = new URL("platform:/plugin/io.snyk.eclipse.plugin/resources/mockCliOutput.json");
      InputStream inputStream = url.openConnection().getInputStream();
      BufferedReader in = new BufferedReader(new InputStreamReader(inputStream));
      String inputLine;
      StringBuilder output = new StringBuilder();

      while ((inputLine = in.readLine()) != null) {
        output.append(inputLine);
      }

      in.close();
      return output.toString();
    } catch (IOException e) {
      SnykLogger.logError(e);
      throw new RuntimeException(e);
    }
  }
}
