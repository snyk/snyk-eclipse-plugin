package io.snyk.eclipse.plugin.utils;

import java.nio.file.Paths;
import java.util.Locale;
import java.util.Map;

/**
 * Supported platform.
 */
public enum Platform {
	LINUX("snyk-linux"),
	LINUX_ALPINE("snyk-alpine"),
	MAC_OS("snyk-macos"),
	WINDOWS("snyk-win.exe");

	public final String snykWrapperFileName;
	
	Platform(String snykWrapperFileName) {
		this.snykWrapperFileName = snykWrapperFileName;
	}

	public static Platform current() throws Exception {
		return detect(System.getProperties());
	}

	private static Platform detect(Map<Object, Object> systemProperties) throws Exception {
		String osArchitecture = ((String) systemProperties.get("os.name")).toLowerCase(Locale.ENGLISH);

		if (osArchitecture.contains("linux")) {
			return Paths.get("/etc/alpine-release").toFile().exists() ? LINUX_ALPINE : LINUX;
		} else if (osArchitecture.contains("mac os x") || osArchitecture.contains("darwin") || osArchitecture.contains("osx")) {
			return MAC_OS;
		} else if (osArchitecture.contains("windows")) {
			return WINDOWS;
		}

		throw new Exception(osArchitecture + " is not supported CPU type");
	}
}
