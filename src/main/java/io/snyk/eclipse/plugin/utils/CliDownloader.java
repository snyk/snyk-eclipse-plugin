package io.snyk.eclipse.plugin.utils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;


import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.BasicResponseHandler;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.client.LaxRedirectStrategy;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.domain.LatestReleaseInfo;

public class CliDownloader {
	
	private static final String LATEST_RELEASES_URL = "https://api.github.com/repos/snyk/snyk/releases/latest";
	private static final String LATEST_RELEASE_DOWNLOAD_URL = "https://github.com/snyk/snyk/releases/download/%s/%s";
	
	private static final String HEADER_CONTENT_LENGTH = "content-length";
	
	public static CliDownloader newInstance() {
		return new CliDownloader();
	}
	
	public File download(File destinationFile, IProgressMonitor monitor) {					
		CloseableHttpClient httpClient = HttpClients
				.custom()
				.setRedirectStrategy(new LaxRedirectStrategy())
				.build();
		
		try {			
			String snykWrapperFileName = Platform.current().snykWrapperFileName;		           
			String cliVersion = getLatestReleaseInfo().getTagName();		   		    
		    
			HttpGet httpGet = new HttpGet(String.format(LATEST_RELEASE_DOWNLOAD_URL, cliVersion, snykWrapperFileName));
			
			File downloadedFile = httpClient.execute(httpGet, new FileDownloadResponseHandler(destinationFile, monitor));
			
			return downloadedFile;
		} catch (Exception exception) {
			throw new IllegalStateException(exception);
		} finally {
			try {
				httpClient.close();
			} catch (IOException ioException) {				
				ioException.printStackTrace();
			}
		}
	}
	
	private LatestReleaseInfo getLatestReleaseInfo() {		
		ObjectMapper objectMapper = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			
		try {
			String latestReleaseInfoStr = requestLatestReleaseInfo();
			
			return objectMapper.readValue(latestReleaseInfoStr, LatestReleaseInfo.class);
		} catch (IOException ioException) {
			ioException.printStackTrace();
		}	
		
		return null;
	}
	
	private String requestLatestReleaseInfo() throws IOException { 		
		return HttpClients.createDefault().execute(new HttpGet(LATEST_RELEASES_URL), new BasicResponseHandler());
	}

	class FileDownloadResponseHandler implements ResponseHandler<File> {

		private final File destinationFile;
		private IProgressMonitor progressMonitor;

		public FileDownloadResponseHandler(File file, IProgressMonitor monitor) {
			this.destinationFile = file;
			this.progressMonitor = monitor;
		}

		@Override
		public File handleResponse(HttpResponse httpResponse) throws ClientProtocolException, IOException {			
			String contentLengthStr = httpResponse.getFirstHeader(HEADER_CONTENT_LENGTH).getValue();
			
            SubMonitor subMonitor = SubMonitor.convert(progressMonitor, Integer.valueOf(contentLengthStr));
			
			InputStream inputStream = httpResponse.getEntity().getContent();
			
			try (FileOutputStream outputStream = new FileOutputStream(destinationFile)) {
	            int readCount;
	            byte[] buffer = new byte[1024];	           

	            while ((readCount = inputStream.read(buffer)) != -1) {
	                outputStream.write(buffer, 0, readCount);
	               	               	               	               
	                subMonitor.split(readCount);	               
	            }	           	            
	        }
			
		    return this.destinationFile;
		}
	}
}
