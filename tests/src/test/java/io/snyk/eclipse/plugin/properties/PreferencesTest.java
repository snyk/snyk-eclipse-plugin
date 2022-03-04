package io.snyk.eclipse.plugin.properties;

import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.internal.security.storage.SecurePreferencesWrapper;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings("restriction")
class PreferencesTest {

    @Test
    void testAuthTokenIsStoredAndRetrievedFromSecureStorage() throws StorageException {
        try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito.mockStatic(SecurePreferencesFactory.class)) {
            ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
            mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
            ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
            when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);

            new Preferences().store(Preferences.AUTH_TOKEN_KEY, "testValue");

            verify(node).put(Preferences.AUTH_TOKEN_KEY, "testValue", true);
            Assertions.assertNull(
                    Platform.getPreferencesService().getString(
                            Preferences.QUALIFIER, Preferences.AUTH_TOKEN_KEY, null, null
                    )
            );
        }
    }

    @Test
    void testGetAuthTokenFromSecureStorage() throws StorageException {
        try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito.mockStatic(SecurePreferencesFactory.class)) {
            ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
            mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
            ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
            when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);
            when(node.get(Preferences.AUTH_TOKEN_KEY, null)).thenReturn("testEncryptedToken");

            new Preferences().getAuthToken();

            verify(node).get(Preferences.AUTH_TOKEN_KEY, null);
        }
    }
}
