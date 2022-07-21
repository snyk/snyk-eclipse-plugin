package io.snyk.eclipse.plugin.properties.store;

import io.snyk.eclipse.plugin.EnvironmentConstants;
import org.apache.commons.lang3.SystemUtils;
import org.eclipse.equinox.internal.security.storage.SecurePreferencesWrapper;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SuppressWarnings("restriction")
class PreferencesTest {

  @BeforeEach
  void setUp() {
    PreferencesUtils.setPreferences(null);
  }

  @Test
	void testSnykCodeEnablementIsStoredAndRetrievedFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(io.snyk.eclipse.plugin.properties.store.Preferences.QUALIFIER)).thenReturn(node);

			Preferences.getInstance().store(io.snyk.eclipse.plugin.properties.store.Preferences.ACTIVATE_SNYK_CODE, "testValue");

			verify(node).put(io.snyk.eclipse.plugin.properties.store.Preferences.ACTIVATE_SNYK_CODE, "testValue", true);
		}
	}

	@Test
	void testSnykOpenSourceEnablementIsStoredAndRetrievedFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(io.snyk.eclipse.plugin.properties.store.Preferences.QUALIFIER)).thenReturn(node);

			Preferences.getInstance().store(io.snyk.eclipse.plugin.properties.store.Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "testValue");

			verify(node).put(io.snyk.eclipse.plugin.properties.store.Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "testValue", true);
		}
	}

	@Test
	void testSnykIacEnablementIsStoredAndRetrievedFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(io.snyk.eclipse.plugin.properties.store.Preferences.QUALIFIER)).thenReturn(node);

			Preferences.getInstance().store(io.snyk.eclipse.plugin.properties.store.Preferences.ACTIVATE_SNYK_IAC, "testValue");

			verify(node).put(io.snyk.eclipse.plugin.properties.store.Preferences.ACTIVATE_SNYK_IAC, "testValue", true);
		}
	}

	@Test
	void testAuthTokenIsStoredAndRetrievedFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(io.snyk.eclipse.plugin.properties.store.Preferences.QUALIFIER)).thenReturn(node);

			Preferences.getInstance().store(io.snyk.eclipse.plugin.properties.store.Preferences.AUTH_TOKEN_KEY, "testValue");

			verify(node).put(io.snyk.eclipse.plugin.properties.store.Preferences.AUTH_TOKEN_KEY, "testValue", true);
		}
	}

	@Test
	void testGetAuthTokenFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(io.snyk.eclipse.plugin.properties.store.Preferences.QUALIFIER)).thenReturn(node);
			when(node.get(eq(io.snyk.eclipse.plugin.properties.store.Preferences.AUTH_TOKEN_KEY), anyString())).thenReturn("testEncryptedToken");

			Preferences.getInstance().getAuthToken();

			verify(node).get(eq(io.snyk.eclipse.plugin.properties.store.Preferences.AUTH_TOKEN_KEY), anyString());
		}
	}

	@Test
	void testGetAuthTokenFromSecureStorageShouldReturnMessageWhenEmpty() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(io.snyk.eclipse.plugin.properties.store.Preferences.QUALIFIER)).thenReturn(node);
			when(node.get(io.snyk.eclipse.plugin.properties.store.Preferences.AUTH_TOKEN_KEY, "")).thenReturn("");

			Preferences.getInstance().getAuthToken();

			verify(node).get(eq(io.snyk.eclipse.plugin.properties.store.Preferences.AUTH_TOKEN_KEY), anyString());
		}
	}

	@Test
	void testExistingTokenInEnvironmentIsStoredInPreferences() throws StorageException {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
					.mockStatic(SecurePreferencesFactory.class)) {
				ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
				mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault)
						.thenReturn(securePreferenceMock);
				ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
				when(securePreferenceMock.node(io.snyk.eclipse.plugin.properties.store.Preferences.QUALIFIER)).thenReturn(node);
				when(node.get(io.snyk.eclipse.plugin.properties.store.Preferences.AUTH_TOKEN_KEY, "")).thenReturn("");

				mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_TOKEN, ""))
						.thenReturn("token");

				new io.snyk.eclipse.plugin.properties.store.Preferences();

				verify(node).put(io.snyk.eclipse.plugin.properties.store.Preferences.AUTH_TOKEN_KEY, "token", true);
			}
		}
	}

	@Test
	void testExistingEndpointInEnvironmentIsStoredInPreferences() throws StorageException {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
					.mockStatic(SecurePreferencesFactory.class)) {
				ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
				mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault)
						.thenReturn(securePreferenceMock);
				ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
				when(securePreferenceMock.node(io.snyk.eclipse.plugin.properties.store.Preferences.QUALIFIER)).thenReturn(node);
				when(node.get(io.snyk.eclipse.plugin.properties.store.Preferences.ENDPOINT_KEY, "")).thenReturn("");

				mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_API, ""))
						.thenReturn("https://custom.endpoint.io");

				new io.snyk.eclipse.plugin.properties.store.Preferences();

				verify(node).put(io.snyk.eclipse.plugin.properties.store.Preferences.ENDPOINT_KEY, "https://custom.endpoint.io", true);
			}
		}
	}

	@Test
	void testExistingOrgInEnvironmentIsStoredInPreferences() throws StorageException {
		try (MockedStatic<SystemUtils> mockedSystemUtils = Mockito.mockStatic(SystemUtils.class)) {
			try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
					.mockStatic(SecurePreferencesFactory.class)) {
				ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
				mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault)
						.thenReturn(securePreferenceMock);
				ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
				when(securePreferenceMock.node(io.snyk.eclipse.plugin.properties.store.Preferences.QUALIFIER)).thenReturn(node);
				when(node.get(io.snyk.eclipse.plugin.properties.store.Preferences.ORGANIZATION_KEY, "")).thenReturn("");

				mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstants.ENV_SNYK_ORG, ""))
						.thenReturn("myOrg");

				new io.snyk.eclipse.plugin.properties.store.Preferences();

				verify(node).put(Preferences.ORGANIZATION_KEY, "myOrg", true);
			}
		}
	}
}
