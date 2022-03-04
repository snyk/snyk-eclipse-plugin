package io.snyk.eclipse.plugin.utils;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.snyk.languageserver.download.LsMetadata;
import org.apache.http.HttpResponse;
import org.apache.http.client.ResponseHandler;

import java.io.IOException;
import java.io.InputStream;

public class LsMetadataResponseHandler implements ResponseHandler<LsMetadata> {
    private final ObjectMapper om = new ObjectMapper();

    public LsMetadataResponseHandler() {
    }

    @Override
    public LsMetadata handleResponse(HttpResponse httpResponse) {
        try (InputStream inputStream = httpResponse.getEntity().getContent()) {
            return om.readValue(inputStream, LsMetadata.class);
        } catch (UnsupportedOperationException | IOException e) {
            throw new RuntimeException(e);
        }
    }
}
