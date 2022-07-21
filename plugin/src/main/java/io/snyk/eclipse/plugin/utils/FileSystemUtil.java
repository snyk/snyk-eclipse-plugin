package io.snyk.eclipse.plugin.utils;

import io.snyk.eclipse.plugin.properties.store.Preferences;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public final class FileSystemUtil {

  /**
   * Get Snyk CLI file depending on OS type.
   * <p>
   * If OS is Windows File will reference to %APPDATA%/Local/Snyk/snyk-win.exe. In
   * other cases File will reference to ~/.snyk/snyk-linux (or snyk-alpine or
   * snyk-macos).
   *
   * @return File
   */
  public static File getCliFile() {
    return new File(Preferences.getInstance().getCliPath());
  }

  /**
   * Get Snyk Binary directory on OS type.
   * <p>
   * If OS is Windows File will reference to %APPDATA%/Local/Snyk directory. In
   * other cases File will reference to ~/.snyk directory.
   *
   * @return File
   */
  public static File getBinaryDirectory() {
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
   * <p>
   * First it try to get system environment variable 'LOCALAPPDATA'. If it's not exists it will
   * try to get directory by 'user.home system property. If this directory not exists it will
   * try to get 'Local Settings/Application Data' directory.
   *
   * @return File
   */
  public static File getWindowsLocalAppDataDirectory() {
    Path localDataPath;

    final String localAppDataEnvVar = System.getenv("LOCALAPPDATA");

    if (localAppDataEnvVar != null) {
      localDataPath = Paths.get(localAppDataEnvVar);

      if (!Files.isDirectory(localDataPath)) {
        throw new RuntimeException("%LOCALAPPDATA% set to nonexistent directory " + localDataPath);
      }
    } else {
      Path userHomePath = Paths.get(System.getProperty("user.home"));

      localDataPath = userHomePath.resolve(Paths.get("AppData", "Local"));

      if (!Files.isDirectory(localDataPath)) {
        localDataPath = userHomePath.resolve(Paths.get("Local Settings", "Application Data"));
      }

      if (!Files.isDirectory(localDataPath)) {
        throw new RuntimeException("%LOCALAPPDATA% is undefined, and neither "
          + userHomePath.resolve(Paths.get("AppData", "Local")) + " nor "
          + userHomePath.resolve(Paths.get("Local Settings", "Application Data")) + " have been found.");
      }
    }

    return localDataPath.toFile();
  }
}
