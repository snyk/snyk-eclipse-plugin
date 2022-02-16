package io.snyk.eclipse.plugin.utils;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.commons.lang.SystemUtils;

public final class FileSystemUtil {

	/**
	 * Get Snyk CLI file depending on OS type.
	 * 
	 * If OS is Windows File will reference to %APPDATA%/Local/Snyk/snyk-win.exe. In
	 * other cases File will reference to ~/.snyk/snyk-linux (or snyk-alpine or
	 * snyk-macos).
	 * 
	 * @throws Exception
	 * 
	 * @return File
	 */
	public static File getCliFile() throws Exception {
		String userHomePath = System.getProperty("user.home");

		File destinationDirectory;

		if (SystemUtils.IS_OS_WINDOWS) {
			destinationDirectory = new File(getWindowsLocalAppDataDirectory(), "Snyk");
		} else {
			destinationDirectory = new File(userHomePath, ".snyk");
		}

		return new File(destinationDirectory, Platform.current().snykWrapperFileName);
	}

	/**
	 * Get Snyk CLI directory on OS type.
	 * 
	 * If OS is Windows File will reference to %APPDATA%/Local/Snyk directory. In
	 * other cases File will reference to ~/.snyk directory.
	 * 
	 * @throws Exception
	 * 
	 * @return File
	 */
	public static File getCliDirectory() throws Exception {
		String userHomePath = System.getProperty("user.home");

		File destinationDirectory;

		if (SystemUtils.IS_OS_WINDOWS) {
			destinationDirectory = new File(getWindowsLocalAppDataDirectory(), "Snyk");
		} else {
			destinationDirectory = new File(userHomePath, ".snyk");
		}

		return destinationDirectory;
	}

	/**
	 * Search %APPDATA%/Local.
	 * 
	 * First it try to get system environment variable 'LOCALAPPDATA'. If it's not exists it will 
	 * try to get directory by 'user.home system property. If this directory not exists it will 
	 * try to get 'Local Settings/Application Data' directory.
	 * 
	 * @throws IOException - throws if directory not found
	 * 
	 * @return File
	 */
	public static File getWindowsLocalAppDataDirectory() throws IOException {
		Path localDataPath;
		
		final String localAppDataEnvVar = System.getenv("LOCALAPPDATA");
		
		if (localAppDataEnvVar != null) {
			localDataPath = Paths.get(localAppDataEnvVar);
			
			if (!Files.isDirectory(localDataPath)) {
				throw new IOException("%LOCALAPPDATA% set to nonexistent directory " + localDataPath);	
			}				
		} else {
			Path userHomePath = Paths.get(System.getProperty("user.home"));

			localDataPath = userHomePath.resolve(Paths.get("AppData", "Local"));

			if (!Files.isDirectory(localDataPath)) {
				localDataPath = userHomePath.resolve(Paths.get("Local Settings", "Application Data"));
			}

			if (!Files.isDirectory(localDataPath)) {
				throw new IOException("%LOCALAPPDATA% is undefined, and neither "
						+ userHomePath.resolve(Paths.get("AppData", "Local")) + " nor "
						+ userHomePath.resolve(Paths.get("Local Settings", "Application Data")) + " have been found.");
			}
		}
		
		return localDataPath.toFile();
	}
}
