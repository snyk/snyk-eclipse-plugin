package io.snyk.languageserver;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.preferences.PreferencesUtils;
import org.eclipse.core.internal.net.ProxyData;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.net.proxy.IProxyService;
import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.security.storage.StorageException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.Version;

import java.util.HashMap;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

class LsRuntimeEnvironmentTest extends LsBaseTest {
	private Preferences preferenceMock = null;

	@Override
	@BeforeEach
	protected void setUp() {
		preferenceMock = mock(Preferences.class);
		PreferencesUtils.setPreferences(preferenceMock);
		environment = new LsRuntimeEnvironment();
	}

	@Test
	void testDownloadBinaryNameConstructions() {
		var actual = environment.getDownloadBinaryName();
		String os = environment.getOs();
		String arch = environment.getArch();
		// Mirror the Windows-ARM fallback: no snyk-win-arm64.exe is published
		if ("win".equals(os) && "-arm64".equals(arch)) {
			arch = "";
		}
		String expected = "snyk-" + os + arch;
		if (expected.contains("win"))
			expected += ".exe";
		assertEquals(expected, actual);
	}

	@Test
	void testDownloadBinaryNameFallsBackToWindowsX64OnArm() {
		// Snyk CLI does not publish a win-arm64 build; Windows-on-ARM emulates x64,
		// so we should download the x64 binary instead of a non-existent arm64 one.
		LsRuntimeEnvironment spyEnv = spy(new LsRuntimeEnvironment());
		doReturn("win").when(spyEnv).getOs();
		doReturn("-arm64").when(spyEnv).getArch();

		assertEquals("snyk-win.exe", spyEnv.getDownloadBinaryName());
	}

	@Test
	void testDownloadBinaryNameKeepsArmSuffixOnMacAndLinux() {
		LsRuntimeEnvironment spyEnv = spy(new LsRuntimeEnvironment());
		doReturn("macos").when(spyEnv).getOs();
		doReturn("-arm64").when(spyEnv).getArch();

		assertEquals("snyk-macos-arm64", spyEnv.getDownloadBinaryName());
	}

	// @Test
	@SuppressWarnings("unchecked")
	void testUpdateEnvironment() {
		try (MockedStatic<Platform> platformMockedStatic = Mockito.mockStatic(Platform.class)) {
			HashMap<String, String> env = new HashMap<>();
			Version version = new Version(42, 42, 42);
			Bundle bundleMock = mock(Bundle.class);
			BundleContext ctxMock = mock(BundleContext.class);
			IProxyService proxyServiceMock = mock(IProxyService.class);
			@SuppressWarnings("restriction")
			IProxyData[] proxyData = new IProxyData[] { new ProxyData("https", "http://localhost", 3128, false, "") };
			platformMockedStatic.when(() -> Platform.getBundle(Activator.PLUGIN_ID)).thenReturn(bundleMock);
			when(bundleMock.getBundleContext()).thenReturn(ctxMock);
			when(bundleMock.getVersion()).thenReturn(version);
			when(ctxMock.getServiceReference(IProxyService.class)).thenReturn(mock(ServiceReference.class));
			when(ctxMock.getService(any())).thenReturn(proxyServiceMock);
			when(proxyServiceMock.getProxyData()).thenReturn(proxyData);

			environment.updateEnvironment(env);

			assertEquals("http://localhost:3128", env.get("https_proxy"));
			assertEquals("ECLIPSE", env.get("SNYK_INTEGRATION_NAME"));
			assertEquals(version.toString(), env.get("SNYK_INTEGRATION_VERSION"));
		}
		assertThrows(NullPointerException.class, () -> Platform.getBundle(Activator.PLUGIN_ID).getVersion());
	}

	@Test
	void testUpdateEnvironmentDoesNotInjectProductEnablement() {
		HashMap<String, String> env = new HashMap<>();
		LsRuntimeEnvironment spyEnv = spy(environment);
		doReturn(null).when(spyEnv).getProxyService();
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE)).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_IAC)).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY)).thenReturn("false");
		when(preferenceMock.getPref(Preferences.ADDITIONAL_PARAMETERS, "")).thenReturn("");
		when(preferenceMock.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "")).thenReturn("");
		when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "false")).thenReturn("false");
		when(preferenceMock.getPath()).thenReturn(java.util.Optional.empty());

		spyEnv.updateEnvironment(env);

		// product enablement is passed via didChangeConfiguration, not process env
		org.junit.jupiter.api.Assertions.assertFalse(env.containsKey(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
		org.junit.jupiter.api.Assertions.assertFalse(env.containsKey(Preferences.ACTIVATE_SNYK_IAC));
		org.junit.jupiter.api.Assertions.assertFalse(env.containsKey(Preferences.ACTIVATE_SNYK_CODE_SECURITY));
	}

	@Test
	void testAddAdditionalParamsAndEnvAddsThemToEnvironment() throws StorageException {
		HashMap<String, String> env = new HashMap<>();
		when(preferenceMock.getPref(Preferences.ADDITIONAL_PARAMETERS, "")).thenReturn("addParams");
		when(preferenceMock.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "")).thenReturn("a=b;c=d;e=f=g");

		environment.addAdditionalParamsAndEnv(env);

		assertEquals("addParams", env.get(Preferences.ADDITIONAL_PARAMETERS));
		assertEquals("b", env.get("a"));
		assertEquals("d", env.get("c"));
		assertEquals("f=g", env.get("e"));
	}

	@Test
	void testSendErrorReportsIsAddedToEnvironment() throws StorageException {
		HashMap<String, String> env = new HashMap<>();
		when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "false")).thenReturn("false");

		environment.addTelemetry(env);

		assertEquals("true", env.get(Preferences.SEND_ERROR_REPORTS));
	}

	@Test
	void testUpdateEnvironmentDoesNotInjectOrganization() {
		HashMap<String, String> env = new HashMap<>();
		LsRuntimeEnvironment spyEnv = spy(environment);
		doReturn(null).when(spyEnv).getProxyService();
		when(preferenceMock.getPref(Preferences.ORGANIZATION_KEY, "")).thenReturn("my-org");
		when(preferenceMock.getPref(Preferences.ADDITIONAL_PARAMETERS, "")).thenReturn("");
		when(preferenceMock.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "")).thenReturn("");
		when(preferenceMock.getPref(Preferences.SEND_ERROR_REPORTS, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "false")).thenReturn("false");
		when(preferenceMock.getPath()).thenReturn(java.util.Optional.empty());

		spyEnv.updateEnvironment(env);

		// organization is passed via didChangeConfiguration, not process env
		org.junit.jupiter.api.Assertions.assertFalse(env.containsKey(Preferences.ORGANIZATION_KEY));
	}

	@Test
	void testEnableTelemetryIsAddedToEnvironment() throws StorageException {
		HashMap<String, String> env = new HashMap<>();
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "true")).thenReturn("true");
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "false")).thenReturn("true");

		environment.addTelemetry(env);
		// This is a bit confusing - CLI takes DISABLE as env variable, but we ask for
		// ENABLE, so it's reverted
		assertEquals("0", env.get(Preferences.ENABLE_TELEMETRY));
	}

	@Test
	void testEnableTelemetryIsAddedToEnvironmentDisabled() throws StorageException {
		HashMap<String, String> env = new HashMap<>();
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "")).thenReturn("false");
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "true")).thenReturn("false");
		when(preferenceMock.getPref(Preferences.ENABLE_TELEMETRY, "false")).thenReturn("false");

		environment.addTelemetry(env);
		// This is a bit confusing - CLI takes DISABLE as env variable, but we ask for
		// ENABLE, so it's reverted
		assertEquals("1", env.get(Preferences.ENABLE_TELEMETRY));
	}

	@Test
	void testAddPath() throws StorageException {
		String expected = "C;/myPath/:";
		HashMap<String, String> env = new HashMap<>();
		when(preferenceMock.getPath()).thenReturn(Optional.of(expected));

		environment.addPath(env);

		assert (env.get("PATH").startsWith(expected));
	}
}
