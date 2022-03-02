package io.snyk.languageserver.download;

import org.apache.http.client.methods.HttpGet;

import java.net.URI;
import java.net.URISyntaxException;


public class LsDownloadRequest extends HttpGet {
    private static final String GITHUB_DOWNLOAD_URL = "https://github.com/snyk/snyk-lsp/releases/download/latest";

    public LsDownloadRequest(LsUtils utils) {
        try {
            setURI(new URI(GITHUB_DOWNLOAD_URL + "/" + utils.getBinaryName(utils.getArch(), utils.getOs())));
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }
}
