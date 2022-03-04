package io.snyk.languageserver.download;

import io.snyk.eclipse.plugin.utils.FileDownloadResponseHandler;
import io.snyk.eclipse.plugin.utils.LsMetadataResponseHandler;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsUtils;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.impl.client.CloseableHttpClient;
import org.eclipse.core.runtime.IProgressMonitor;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.stream.Collectors;

public class LsDownloader {
    private final CloseableHttpClient httpClient;
    private final LsUtils utils;

    public LsDownloader(LsUtils utils, CloseableHttpClient httpClient) {
        this.httpClient = httpClient;
        this.utils = utils;
    }

    public void download(IProgressMonitor monitor) {
        File destinationFile = utils.getLSFile();
        File tempFile = null;
        try {
            tempFile = File.createTempFile(destinationFile.getName(), ".tmp", destinationFile.getParentFile());
            var version = getVersion();
            var expectedSha = getSha(version);

            LsDownloadRequest binaryRequest = new LsDownloadRequest(version, utils);
            tempFile = httpClient.execute(binaryRequest, new FileDownloadResponseHandler(tempFile, monitor));

            var checksumDownloadedFile = Files.readAllBytes(tempFile.toPath());
            verifyChecksum(expectedSha, checksumDownloadedFile);

            try {
                Files.move(tempFile.toPath(), destinationFile.toPath(), StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
            } catch (AtomicMoveNotSupportedException e) {
                // fallback to renameTo because of e
                if (!tempFile.renameTo(destinationFile)) throw new IOException("Rename not successful");
            }
            if (!destinationFile.setExecutable(true))
                throw new IOException("Could not set executable permission on " + destinationFile);
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            try {
                if (tempFile != null && tempFile.exists()) if (!tempFile.delete()) tempFile.deleteOnExit();
            } catch (Exception e) {
                SnykLogger.logError(e);
            }
        }
    }

    String getVersion() {
        LsVersionRequest req = new LsVersionRequest();
        LsMetadata metadata;
        try {
            metadata = httpClient.execute(req, new LsMetadataResponseHandler());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return metadata.getVersion();
    }

    String getSha(String version) {
        LsShaRequest shaRequest = new LsShaRequest(version, utils);
        try (CloseableHttpResponse response = httpClient.execute(shaRequest)) {
            if (response.getStatusLine().getStatusCode() >= 400) {
                throw new RuntimeException("Download of Language Server failed. " + response.getStatusLine());
            }

            var entity = response.getEntity();
            try (var buf = new BufferedReader(new InputStreamReader(entity.getContent(), StandardCharsets.UTF_8))) {
                String fileName = utils.getDownloadBinaryName(version);
                List<String> lines = buf.lines().filter(s -> s.contains(fileName)).collect(Collectors.toList());
                if (lines.size() != 1)
                    throw new ChecksumVerificationException("Could not find sha for verification of file: " + fileName);
                return lines.get(0).split(" ")[0];
            }
        } catch (UnsupportedOperationException | IOException e) {
            throw new ChecksumVerificationException(e);
        }
    }

    private void verifyChecksum(String expectedSha, byte[] checksumDownloadedFile) {
        try {
            byte[] sha = MessageDigest.getInstance("SHA-256").digest(checksumDownloadedFile);
            String actualSha = bytesToHex(sha).toLowerCase();
            if (!actualSha.equalsIgnoreCase(expectedSha)) {
                throw new ChecksumVerificationException("Expected " + expectedSha + ", but downloaded file has " + actualSha);
            }
        } catch (NoSuchAlgorithmException e) {
            throw new ChecksumVerificationException(e);
        }
    }

    private static String bytesToHex(byte[] hash) {
        StringBuilder hexString = new StringBuilder(2 * hash.length);
        for (byte b : hash) {
            String hex = Integer.toHexString(0xff & b);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }
}
