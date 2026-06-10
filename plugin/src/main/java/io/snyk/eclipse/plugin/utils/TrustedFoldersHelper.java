package io.snyk.eclipse.plugin.utils;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.stream.Collectors;

import io.snyk.eclipse.plugin.preferences.Preferences;

public final class TrustedFoldersHelper {

	private TrustedFoldersHelper() {
		throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
	}

	public static void addTrustedFolders(String... paths) {
		var prefs = Preferences.getInstance();
		var stored = prefs.getPref(Preferences.TRUSTED_FOLDERS, "");
		var pathSet = stored.isBlank() ? new HashSet<String>()
				: new HashSet<>(Arrays.asList(stored.split(File.pathSeparator)));
		pathSet.addAll(Arrays.asList(paths));
		// pathSet is a HashSet — already deduplicated; no .distinct() needed.
		prefs.store(Preferences.TRUSTED_FOLDERS, pathSet.stream()
				.filter(s -> !s.isBlank())
				.map(String::trim)
				.collect(Collectors.joining(File.pathSeparator)));
	}
}
