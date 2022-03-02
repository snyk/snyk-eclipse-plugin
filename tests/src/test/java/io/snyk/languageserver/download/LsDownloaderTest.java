package io.snyk.languageserver.download;

import io.snyk.eclipse.plugin.utils.FileDownloadResponseHandler;
import org.apache.http.HttpEntity;
import org.apache.http.StatusLine;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.impl.client.CloseableHttpClient;
import org.eclipse.core.runtime.IProgressMonitor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.stubbing.OngoingStubbing;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.CopyOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

public class LsDownloaderTest extends LsBaseTest {
    CloseableHttpClient httpClient = mock(CloseableHttpClient.class);
    
    @BeforeEach
    @Override
    void setUp() {
    	super.setUp();
    	httpClient = mock(CloseableHttpClient.class);
    }

    @Test
    void downloadShouldFailWhenShaWrongAndFileShouldNotBeOverwritten() throws IOException {
        byte[] expectedLsContent = Files.readAllBytes(utils.getLSFile().toPath());
        LsDownloader cut = new LsDownloader(utils, httpClient);
        File testFile = File.createTempFile("download-test", "tmp");
        testFile.deleteOnExit();
        // sha corresponds to file content (`echo "test 123" | sha256sum`)
        Files.write(testFile.toPath(), "test 123".getBytes(StandardCharsets.UTF_8));
        InputStream shaStream =
                new ByteArrayInputStream("wrong sha".getBytes());
        mockShaStream(shaStream, 200);
        when(httpClient.execute(any(LsDownloadRequest.class), any(FileDownloadResponseHandler.class))).thenReturn(testFile);

        assertThrows(ChecksumVerificationException.class, () -> cut.download(mock(IProgressMonitor.class)));

        File lsFile = utils.getLSFile();
        assertTrue(lsFile.exists());
        assertArrayEquals(Files.readAllBytes(lsFile.toPath()), expectedLsContent);
    }

    private void mockShaStream(InputStream shaStream, int statusCode) throws IOException {
        CloseableHttpResponse shaHttpResponseMock = mock(CloseableHttpResponse.class);
        when(httpClient.execute(any(LsShaDownloadRequest.class))).thenReturn(shaHttpResponseMock);
        StatusLine statusLineMock = mock(StatusLine.class);
		when(shaHttpResponseMock.getStatusLine()).thenReturn(statusLineMock);
		when(statusLineMock.getStatusCode()).thenReturn(statusCode);
        HttpEntity shaHttpEntityMock = mock(HttpEntity.class);
        when(shaHttpResponseMock.getEntity()).thenReturn(shaHttpEntityMock);
        when(shaHttpEntityMock.getContent()).thenReturn(shaStream);
    }

    @Test
    void downloadShouldIssueDownloadRequestForShaAndBinary() throws IOException {
        LsDownloader cut = new LsDownloader(utils, httpClient);
        Path source = Paths.get("src/test/resources/ls-dummy-binary");
        var testFile = Files.createTempFile("download-test", "tmp").toFile();
        testFile.deleteOnExit();
        Files.copy(source, testFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
        var fileContent = Files.readAllBytes(testFile.toPath());
        InputStream shaStream =
                new ByteArrayInputStream("2b418a5d0573164b4f93188fc94de0332fc0968e7a8439b01f530a4cdde1dcf2".getBytes());
        mockShaStream(shaStream, 200);
        when(httpClient.execute(any(LsDownloadRequest.class), any(FileDownloadResponseHandler.class))).thenReturn(testFile);

        cut.download(mock(IProgressMonitor.class));

        verify(httpClient, times(1)).execute(any(LsDownloadRequest.class), any(FileDownloadResponseHandler.class));
        verify(httpClient, times(1)).execute(any(LsShaDownloadRequest.class));

        File lsFile = utils.getLSFile();
        assertTrue(lsFile.exists());
        assertArrayEquals(Files.readAllBytes(lsFile.toPath()), fileContent);
    }
}
