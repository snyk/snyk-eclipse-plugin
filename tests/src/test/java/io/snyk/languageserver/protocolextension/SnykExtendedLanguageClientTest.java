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
  
  @BeforeEach
  void setUp() {
    store = new InMemoryPreferenceStore();
    PreferencesUtils.setPreferences(Preferences.getInstance(store));
  }

  @Test
  void testAddTrustedPathsAddsPathToPreferenceStore() {
    SnykTrustedFoldersParams param = new SnykTrustedFoldersParams();
    param.setTrustedFolders(new String[] {"trusted/path "});
            
    cut.addTrustedPaths(param);
    
    assertEquals("trusted/path", store.getString(Preferences.TRUSTED_FOLDERS, ""));
  }
  
  @Test
  void testAddTrustedPathsDeduplicatesAndTrims() {
    SnykTrustedFoldersParams param = new SnykTrustedFoldersParams();
    param.setTrustedFolders(new String[] {"trusted/path", "trusted/path", " trusted/path "});  
    
    cut.addTrustedPaths(param);
    
    assertEquals("trusted/path", store.getString(Preferences.TRUSTED_FOLDERS, ""));
  }

  @Test
  void testAddApiTokenToPreferencesStore() {
    HasAuthenticatedParam param = new HasAuthenticatedParam();
    String expectedToken = "apitoken";
    
    param.setToken(expectedToken);

    cut.hasAuthenticated(param);
    
    assertEquals(expectedToken, store.getString(Preferences.AUTH_TOKEN_KEY, ""));    
  }
  
  @Test
  void testSetsAuthenticationMethodForApiToken() {
    HasAuthenticatedParam param = new HasAuthenticatedParam();
    String expectedToken = "apitoken";
    
    param.setToken(expectedToken);

    cut.hasAuthenticated(param);
    
    assertEquals(Preferences.AUTH_METHOD_TOKEN, store.getString(Preferences.AUTHENTICATION_METHOD, ""));
  }
  
  @Test
  void testSetsAuthenticationMethodForOAuthToken() {
    HasAuthenticatedParam param = new HasAuthenticatedParam();
    String expectedToken = "{\"access_token\":\"configAccessToken\",\"token_type\":\"Bearer\",\"refresh_token\":\"configRefreshToken\",\"expiry\":\"3023-03-29T17:47:13.714448+02:00\"}";
    
    param.setToken(expectedToken);

    cut.hasAuthenticated(param);
    
    assertEquals(Preferences.AUTH_METHOD_OAUTH, store.getString(Preferences.AUTHENTICATION_METHOD, ""));
  }
}
