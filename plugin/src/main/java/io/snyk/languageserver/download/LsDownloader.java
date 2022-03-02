package io.snyk.languageserver.download;

import io.snyk.eclipse.plugin.utils.FileDownloadResponseHandler;
import io.snyk.eclipse.plugin.utils.SnykLogger;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.impl.client.CloseableHttpClient;
import org.eclipse.core.runtime.IProgressMonitor;

import java.io.File;
import java.io.IOException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class LsDownloader {
	private final CloseableHttpClient httpClient;
	private final LsUtils utils;

	public LsDownloader(LsUtils utils, CloseableHttpClient httpClient) {
		this.httpClient = httpClient;
		this.utils = utils;
	}

	@SuppressWarnings("ResultOfMethodCallIgnored")
	public void download(IProgressMonitor monitor) {
		LsDownloadRequest binaryRequest = new LsDownloadRequest(utils);
		LsShaDownloadRequest shaRequest = new LsShaDownloadRequest(utils);
		File destinationFile = utils.getLSFile();
		File tempFile = null;
		try {
			tempFile = File.createTempFile(destinationFile.getName(), ".tmp", destinationFile.getParentFile());
			CloseableHttpResponse response = httpClient.execute(shaRequest);
			if (response.getStatusLine().getStatusCode() >= 400) {
				throw new RuntimeException("Download of Language Server failed. " + response.getStatusLine());
			}

			var entity = response.getEntity();
			var expectedSha = new String(entity.getContent().readAllBytes()); // for a sha content encoding should not
																				// matter;
			tempFile = httpClient.execute(binaryRequest, new FileDownloadResponseHandler(tempFile, monitor));
			

			var checksumDownloadedFile = Files.readAllBytes(tempFile.toPath());
			verifyChecksum(expectedSha, checksumDownloadedFile);

			try {
				Files.move(tempFile.toPath(), destinationFile.toPath(), StandardCopyOption.ATOMIC_MOVE,
						StandardCopyOption.REPLACE_EXISTING);
			} catch (AtomicMoveNotSupportedException e) {
				// fallback to renameTo because of e
				tempFile.renameTo(destinationFile);
			}
			destinationFile.setExecutable(true);
		} catch (IOException e) {
			throw new RuntimeException(e);
		} finally {
			try {
				if (tempFile != null && tempFile.exists())
					tempFile.delete();
			} catch (Exception e) {
				SnykLogger.logError(e);
			}
		}
	}

	private void verifyChecksum(String expectedSha, byte[] checksumDownloadedFile) {
		try {
			byte[] sha = MessageDigest.getInstance("SHA-256").digest(checksumDownloadedFile);
			String actualSha = bytesToHex(sha).toLowerCase();
			if (!actualSha.equalsIgnoreCase(expectedSha)) {
				throw new ChecksumVerificationException(
						"Expected " + expectedSha + ", but downloaded file has " + actualSha);
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
