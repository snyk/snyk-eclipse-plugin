package io.snyk.eclipse.plugin.properties.preferences;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.equinox.security.storage.StorageException;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.LsBaseTest;

class SecurePreferenceStoreTest extends LsBaseTest {

	@Test
	void usesSecureStorageByDefault() throws StorageException {
		prefs.store(Preferences.AUTH_TOKEN_KEY, "testValue");
		assertEquals("testValue", prefs.getSecureStore().getString(Preferences.AUTH_TOKEN_KEY));
		assertEquals("", prefs.getInsecureStore().getString(Preferences.AUTH_TOKEN_KEY));
	}
}
