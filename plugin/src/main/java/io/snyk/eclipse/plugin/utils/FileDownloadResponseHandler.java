package io.snyk.eclipse.plugin.utils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;

import org.apache.http.HttpResponse;
import org.apache.http.client.ResponseHandler;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;

public class FileDownloadResponseHandler implements ResponseHandler<File> {

	private final File destinationFile;
	private final IProgressMonitor progressMonitor;

	public FileDownloadResponseHandler(File file, IProgressMonitor monitor) {
		this.destinationFile = file;
		this.progressMonitor = monitor;
	}

	@Override
	public File handleResponse(HttpResponse httpResponse) throws IOException {
		long contentLengthStr = httpResponse.getEntity().getContentLength();
		SubMonitor subMonitor = SubMonitor.convert(progressMonitor, 100);
		InputStream inputStream = httpResponse.getEntity().getContent();

		try (var outputStream = Files.newOutputStream(destinationFile.toPath())) {
			int readCount = 0;
			byte[] buffer = new byte[1024];

			while (readCount != -1) {
				readCount = inputStream.read(buffer);
				outputStream.write(buffer, 0, readCount);
				subMonitor.split(Math.round((float) readCount / (float) contentLengthStr));
			}
		}

		return this.destinationFile;
	}
}
