package io.snyk.languageserver.protocolextension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.properties.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;

class SnykExtendedLanguageClientTest {
  private InMemoryPreferenceStore store = new InMemoryPreferenceStore();
  private SnykExtendedLanguageClient cut = new SnykExtendedLanguageClient();
  private Preferences pref;

  @BeforeEach
  void setUp() {
    store = new InMemoryPreferenceStore();
    pref = Preferences.getInstance(store);
    PreferencesUtils.setPreferences(pref);
  }

  @Test
  void testAddTrustedPathsAddsPathToPreferenceStore() {
    SnykTrustedFoldersParams param = new SnykTrustedFoldersParams();
    param.setTrustedFolders(new String[] { "trusted/path " });

    cut.addTrustedPaths(param);

    assertEquals("trusted/path", store.getString(Preferences.TRUSTED_FOLDERS, ""));
  }

  @Test
  void testAddTrustedPathsDeduplicatesAndTrims() {
    SnykTrustedFoldersParams param = new SnykTrustedFoldersParams();
    param.setTrustedFolders(new String[] { "trusted/path", "trusted/path", " trusted/path " });

    cut.addTrustedPaths(param);

    assertEquals("trusted/path", store.getString(Preferences.TRUSTED_FOLDERS, ""));
  }

}
