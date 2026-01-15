package io.snyk.eclipse.plugin.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.stream.Collectors;

import org.apache.http.HttpResponse;
import org.apache.http.client.ResponseHandler;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;

public class FileDownloadResponseHandler implements ResponseHandler<File> {

	private final File destinationFile;
	private final IProgressMonitor progressMonitor;
	private final String downloadUrl;
	private final ILog logger;

	public FileDownloadResponseHandler(File file, IProgressMonitor monitor, String downloadUrl) {
		this.destinationFile = file;
		this.progressMonitor = monitor;
		this.downloadUrl = downloadUrl;
		this.logger = Platform.getLog(getClass());
	}

	@Override
	public File handleResponse(HttpResponse httpResponse) throws IOException {
		int statusCode = httpResponse.getStatusLine().getStatusCode();

		if (statusCode < 200 || statusCode > 299) {
			handleHttpError(httpResponse, statusCode);
		}

		long contentLengthStr = httpResponse.getEntity().getContentLength();
		SubMonitor subMonitor = SubMonitor.convert(progressMonitor, 100);
		try (InputStream inputStream = httpResponse.getEntity().getContent();
				var outputStream = Files.newOutputStream(destinationFile.toPath())) {
			int readCount;
			byte[] buffer = new byte[1024];

			while ((readCount = inputStream.read(buffer)) != -1) { // NOPMD by bdoetsch on 3/11/25, 2:29 PM
				outputStream.write(buffer, 0, readCount);
				subMonitor.split(Math.round((float) readCount / (float) contentLengthStr));
			}
		}
		return this.destinationFile;
	}

	private void handleHttpError(HttpResponse httpResponse, int statusCode) throws IOException {
		String reasonPhrase = httpResponse.getStatusLine().getReasonPhrase();
		String responseBody = readResponseBody(httpResponse);

		// Log the response body for debugging (may contain useful error details)
		logger.warn(String.format("CLI download failed. URL: %s, Status: %d %s, Response body: %s",
				downloadUrl, statusCode, reasonPhrase, responseBody));

		// Throw exception with URL and status (but not the body - may be large, may be binary or may be HTML we accidentally render)
		throw new IOException(String.format(
				"Download failed with HTTP %d %s for URL: %s",
				statusCode, reasonPhrase, downloadUrl));
	}

	private String readResponseBody(HttpResponse httpResponse) {
		try {
			if (httpResponse.getEntity() == null) {
				return "(no response body)";
			}
			try (BufferedReader reader = new BufferedReader(
					new InputStreamReader(httpResponse.getEntity().getContent(), StandardCharsets.UTF_8))) {
				return reader.lines().collect(Collectors.joining("\n"));
			}
		} catch (Exception e) {
			return "(could not read response body: " + e.getMessage() + ")";
		}
	}
}
