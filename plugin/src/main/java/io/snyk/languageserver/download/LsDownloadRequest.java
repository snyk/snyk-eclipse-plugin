package io.snyk.languageserver.download;

import io.snyk.languageserver.LsRuntimeEnvironment;
import org.apache.http.client.methods.HttpGet;

import java.net.URI;
import java.net.URISyntaxException;


public class LsDownloadRequest extends HttpGet {
    private static final String DOWNLOAD_URL = "https://static.snyk.io/snyk-ls/%s";

    public LsDownloadRequest(String version, LsRuntimeEnvironment utils) {
        try {
            var binary = utils.getDownloadBinaryName(version);
            var effectiveURL = String.format(DOWNLOAD_URL, binary);
            setURI(new URI(effectiveURL));
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }

    public static String getBaseURL() {
        return DOWNLOAD_URL.replace("%s", "");
    }
}
