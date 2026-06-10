package io.snyk.eclipse.plugin.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.HashSet;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.Preferences;

class TrustedFoldersHelperTest {

	private Preferences prefs;

	@BeforeEach
	void setUp() {
		prefs = Preferences.getInstance();
		prefs.store(Preferences.TRUSTED_FOLDERS, "");
	}

	@Test
	void addsTrustedFolder() {
		TrustedFoldersHelper.addTrustedFolders("/some/path");
		assertEquals("/some/path", prefs.getPref(Preferences.TRUSTED_FOLDERS, ""));
	}

	@Test
	void deduplicatesPaths() {
		TrustedFoldersHelper.addTrustedFolders("/some/path", "/some/path", "/some/path");
		assertEquals("/some/path", prefs.getPref(Preferences.TRUSTED_FOLDERS, ""));
	}

	@Test
	void trimsPaths() {
		TrustedFoldersHelper.addTrustedFolders("  /some/path  ");
		assertEquals("/some/path", prefs.getPref(Preferences.TRUSTED_FOLDERS, ""));
	}

	@Test
	void filtersBlankPaths() {
		TrustedFoldersHelper.addTrustedFolders("", "  ", "/some/path");
		assertEquals("/some/path", prefs.getPref(Preferences.TRUSTED_FOLDERS, ""));
	}

	@Test
	void mergesWithExistingPaths() {
		prefs.store(Preferences.TRUSTED_FOLDERS, "/existing/path");
		TrustedFoldersHelper.addTrustedFolders("/new/path");
		String stored = prefs.getPref(Preferences.TRUSTED_FOLDERS, "");
		var paths = new HashSet<>(Arrays.asList(stored.split(",")));
		assertTrue(paths.contains("/existing/path"));
		assertTrue(paths.contains("/new/path"));
	}

	@Test
	void doesNotDuplicateExistingPaths() {
		prefs.store(Preferences.TRUSTED_FOLDERS, "/existing/path");
		TrustedFoldersHelper.addTrustedFolders("/existing/path");
		assertEquals("/existing/path", prefs.getPref(Preferences.TRUSTED_FOLDERS, ""));
	}
}
