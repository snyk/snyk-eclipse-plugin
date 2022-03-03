package io.snyk.eclipse.plugin.utils;

import org.apache.http.HttpResponse;
import org.apache.http.client.ResponseHandler;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.languageserver.download.LsMetadata;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class LsMetadataResponseHandler implements ResponseHandler<LsMetadata> {
	private ObjectMapper om = new ObjectMapper();

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
