package io.snyk.eclipse.plugin.properties;

import org.eclipse.equinox.internal.security.storage.SecurePreferencesWrapper;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import io.snyk.eclipse.plugin.EnvironmentConstant;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.commons.lang3.SystemUtils;

@SuppressWarnings("restriction")
class PreferencesTest {

	@Test
	void testSnykCodeEnablementIsStoredAndRetrievedFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);

			new Preferences().store(Preferences.ACTIVATE_SNYK_CODE, "testValue");

			verify(node).put(Preferences.ACTIVATE_SNYK_CODE, "testValue", true);
		}
	}

	@Test
	void testSnykOpenSourceEnablementIsStoredAndRetrievedFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);

			new Preferences().store(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "testValue");

			verify(node).put(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "testValue", true);
		}
	}

	@Test
	void testSnykIacEnablementIsStoredAndRetrievedFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);

			new Preferences().store(Preferences.ACTIVATE_SNYK_IAC, "testValue");

			verify(node).put(Preferences.ACTIVATE_SNYK_IAC, "testValue", true);
		}
	}

	@Test
	void testAuthTokenIsStoredAndRetrievedFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);

			new Preferences().store(Preferences.AUTH_TOKEN_KEY, "testValue");

			verify(node).put(Preferences.AUTH_TOKEN_KEY, "testValue", true);
		}
	}

	@Test
	void testGetAuthTokenFromSecureStorage() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);
			when(node.get(eq(Preferences.AUTH_TOKEN_KEY), anyString())).thenReturn("testEncryptedToken");

			new Preferences().getAuthToken();

			verify(node).get(eq(Preferences.AUTH_TOKEN_KEY), anyString());
		}
	}

	@Test
	void testGetAuthTokenFromSecureStorageShouldReturnMessageWhenEmpty() throws StorageException {
		try (MockedStatic<SecurePreferencesFactory> mockedSecurePreferencesFactory = Mockito
				.mockStatic(SecurePreferencesFactory.class)) {
			ISecurePreferences securePreferenceMock = Mockito.mock(ISecurePreferences.class);
			mockedSecurePreferencesFactory.when(SecurePreferencesFactory::getDefault).thenReturn(securePreferenceMock);
			ISecurePreferences node = Mockito.mock(SecurePreferencesWrapper.class);
			when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);
			when(node.get(Preferences.AUTH_TOKEN_KEY, "")).thenReturn("");

			new Preferences().getAuthToken();

			verify(node).get(eq(Preferences.AUTH_TOKEN_KEY), anyString());
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
				when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);
				when(node.get(Preferences.AUTH_TOKEN_KEY, "")).thenReturn("");

				mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstant.ENV_SNYK_TOKEN, ""))
						.thenReturn("token");
				
				new Preferences();

				verify(node).put(Preferences.AUTH_TOKEN_KEY, "token", true);
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
				when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);
				when(node.get(Preferences.ENDPOINT_KEY, "")).thenReturn("");

				mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstant.ENV_SNYK_API, ""))
						.thenReturn("https://custom.endpoint.io");
				
				new Preferences();

				verify(node).put(Preferences.ENDPOINT_KEY, "https://custom.endpoint.io", true);
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
				when(securePreferenceMock.node(Preferences.QUALIFIER)).thenReturn(node);
				when(node.get(Preferences.ORGANIZATION_KEY, "")).thenReturn("");

				mockedSystemUtils.when(() -> SystemUtils.getEnvironmentVariable(EnvironmentConstant.ENV_SNYK_ORG, ""))
						.thenReturn("myOrg");
				
				new Preferences();

				verify(node).put(Preferences.ORGANIZATION_KEY, "myOrg", true);
			}
		}
	}
}
