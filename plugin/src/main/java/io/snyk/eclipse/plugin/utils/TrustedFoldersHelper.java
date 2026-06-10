package io.snyk.eclipse.plugin.utils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import io.snyk.eclipse.plugin.preferences.Preferences;

public final class TrustedFoldersHelper {

	private TrustedFoldersHelper() {
		throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
	}

	private static final String SEPARATOR = ",";

	public static void addTrustedFolders(String... paths) {
		var prefs = Preferences.getInstance();
		var stored = prefs.getPref(Preferences.TRUSTED_FOLDERS, "");
		Set<String> pathSet = stored.isBlank() ? new HashSet<>()
				: new HashSet<>(Arrays.asList(stored.split(SEPARATOR)));
		for (String path : paths) {
			String trimmed = path.trim();
			if (!trimmed.isBlank()) {
				pathSet.add(trimmed);
			}
		}
		prefs.store(Preferences.TRUSTED_FOLDERS, String.join(SEPARATOR, pathSet));
	}
}
