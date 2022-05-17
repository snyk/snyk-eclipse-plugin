package io.snyk.eclipse.plugin.utils;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.snyk.eclipse.plugin.domain.LatestReleaseInfo;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.BasicResponseHandler;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.client.LaxRedirectStrategy;
import org.eclipse.core.runtime.IProgressMonitor;

import java.io.File;
import java.io.IOException;
import java.util.Objects;

public class CliDownloader {

  private static final String LATEST_RELEASES_URL = "https://api.github.com/repos/snyk/snyk/releases/latest";
  private static final String LATEST_RELEASE_DOWNLOAD_URL = "https://github.com/snyk/snyk/releases/download/%s/%s";

  public static CliDownloader newInstance() {
    return new CliDownloader();
  }

  public File download(File destinationFile, IProgressMonitor monitor) {
    CloseableHttpClient httpClient = HttpClients
      .custom()
      .setRedirectStrategy(new LaxRedirectStrategy())
      .build();

    try {
      String snykWrapperFileName = Platform.current().snykWrapperFileName;
      String cliVersion = Objects.requireNonNull(getLatestReleaseInfo()).getTagName();

      HttpGet httpGet = new HttpGet(String.format(LATEST_RELEASE_DOWNLOAD_URL, cliVersion, snykWrapperFileName));

      return httpClient.execute(httpGet, new FileDownloadResponseHandler(destinationFile, monitor));
    } catch (Exception exception) {
      throw new IllegalStateException(exception);
    } finally {
      try {
        httpClient.close();
      } catch (IOException ioException) {
        SnykLogger.logError(ioException);
      }
    }
  }

  private LatestReleaseInfo getLatestReleaseInfo() {
    ObjectMapper objectMapper = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

    try {
      String latestReleaseInfoStr = requestLatestReleaseInfo();

      return objectMapper.readValue(latestReleaseInfoStr, LatestReleaseInfo.class);
    } catch (IOException ioException) {
      SnykLogger.logError(ioException);
    }

    return null;
  }

  private String requestLatestReleaseInfo() throws IOException {
    return HttpClients.createDefault().execute(new HttpGet(LATEST_RELEASES_URL), new BasicResponseHandler());
  }
}
