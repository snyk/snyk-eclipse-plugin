package io.snyk.eclipse.plugin.properties.preferences;

import org.eclipse.equinox.internal.security.storage.SecurePreferencesWrapper;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.preferences.SecurePreferenceStore;
import io.snyk.languageserver.LsBaseTest;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class SecurePreferenceStoreTest extends LsBaseTest {

  @SuppressWarnings("restriction")
@Test
  void usesSecureStorageByDefault() throws StorageException {
    PreferencesUtils.reset();
    try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
      .mockStatic(SecurePreferencesFactory.class)) {
      ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
      mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
      ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
      when(securePreferenceMock.node(SecurePreferenceStore.QUALIFIER)).thenReturn(node);

      Preferences.getInstance().store(Preferences.AUTH_TOKEN_KEY, "testValue");

      verify(node).put(Preferences.AUTH_TOKEN_KEY, "testValue", true);
    }
  }
}
