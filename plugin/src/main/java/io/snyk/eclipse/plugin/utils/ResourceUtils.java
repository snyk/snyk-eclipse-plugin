package io.snyk.eclipse.plugin.utils;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Path;
import java.util.Base64;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.FileLocator;
import org.osgi.framework.Bundle;

public class ResourceUtils {

	public ResourceUtils() {
	}

	public static String getBase64Image(Bundle bundle, String icon) {
		URL imageUrl = FileLocator.find(bundle, new org.eclipse.core.runtime.Path("icons/" + icon), null);

		byte[] imageData = getImageDataFromUrl(imageUrl);

		String base64Image = Base64.getEncoder().encodeToString(imageData);
		return base64Image;
	}

	private static byte[] getImageDataFromUrl(URL imageUrl) {
		try {
			ByteArrayOutputStream output = new ByteArrayOutputStream();

			try (InputStream inputStream = imageUrl.openStream()) {
				int n = 0;
				byte[] buffer = new byte[1024];
				while (-1 != (n = inputStream.read(buffer))) {
					output.write(buffer, 0, n);
				}
			}

			return output.toByteArray();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public static Path getFullPath(IResource resource) {	
		return resource.getLocation().toPath().toAbsolutePath();
	}
}