package io.snyk.languageserver.download;

import io.snyk.languageserver.LsUtils;
import org.apache.http.client.methods.HttpGet;

import java.net.URI;
import java.net.URISyntaxException;


public class LsDownloadRequest extends HttpGet {
    private static final String DOWNLOAD_URL = "https://static.snyk.io/snyk-ls/%s";

    public LsDownloadRequest(String version, LsUtils utils) {
        try {
            var binary = utils.getDownloadBinaryName(version);
            var effectiveURL = String.format(DOWNLOAD_URL, binary);
            setURI(new URI(effectiveURL));
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }
}
