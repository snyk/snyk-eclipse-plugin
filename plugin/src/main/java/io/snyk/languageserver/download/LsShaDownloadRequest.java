package io.snyk.languageserver.download;

import org.apache.http.client.methods.HttpGet;

import java.net.URI;
import java.net.URISyntaxException;


public class LsShaDownloadRequest extends HttpGet {
    private static final String GITHUB_DOWNLOAD_URL = "https://github.com/snyk/snyk-lsp/releases/download/latest";

    public LsShaDownloadRequest(LsUtils utils) {
        try {
            setURI(new URI(GITHUB_DOWNLOAD_URL + "/" + utils.getBinaryName(utils.getArch(), utils.getOs())+".sha256"));
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }
}
