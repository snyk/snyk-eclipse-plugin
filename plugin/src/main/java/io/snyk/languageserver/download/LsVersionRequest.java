package io.snyk.languageserver.download;

import org.apache.http.client.methods.HttpGet;

import java.net.URI;
import java.net.URISyntaxException;


public class LsVersionRequest extends HttpGet {
    private static final String DOWNLOAD_URL = "https://static.snyk.io/snyk-ls/%s";

    public LsVersionRequest() {
        try {
        	var effectiveURL = String.format(DOWNLOAD_URL, "metadata.json");        	
            setURI(new URI(effectiveURL));
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }
}
