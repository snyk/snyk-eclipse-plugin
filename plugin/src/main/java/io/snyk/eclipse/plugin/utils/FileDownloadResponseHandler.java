package io.snyk.eclipse.plugin.utils;

import org.apache.http.HttpResponse;
import org.apache.http.client.ResponseHandler;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class FileDownloadResponseHandler implements ResponseHandler<File> {

    private final File destinationFile;
    private final IProgressMonitor progressMonitor;
    private static final String HEADER_CONTENT_LENGTH = "content-length";

    public FileDownloadResponseHandler(File file, IProgressMonitor monitor) {
        this.destinationFile = file;
        this.progressMonitor = monitor;
    }

    @Override
    public File handleResponse(HttpResponse httpResponse) throws IOException {
        long contentLengthStr = httpResponse.getEntity().getContentLength();
        SubMonitor subMonitor = SubMonitor.convert(progressMonitor, 100);
        InputStream inputStream = httpResponse.getEntity().getContent();

        try (FileOutputStream outputStream = new FileOutputStream(destinationFile)) {
            int readCount;
            byte[] buffer = new byte[1024];

            while ((readCount = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, readCount);

                //noinspection IntegerDivisionInFloatingPointContext
                subMonitor.split(Math.round(readCount/contentLengthStr));
            }
        }

        return this.destinationFile;
    }
}
