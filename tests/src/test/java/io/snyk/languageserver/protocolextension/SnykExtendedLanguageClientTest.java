package io.snyk.languageserver.protocolextension;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.concurrent.TimeUnit;

import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.ShowDocumentParams;
import org.eclipse.lsp4j.ShowDocumentResult;
import org.eclipse.lsp4j.WorkDoneProgressBegin;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.WorkDoneProgressNotification;
import org.eclipse.lsp4j.WorkDoneProgressReport;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import io.snyk.eclipse.plugin.analytics.TaskProcessor;
import io.snyk.eclipse.plugin.preferences.HTMLSettingsPreferencePage;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.LspConfigurationParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;

import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_IN_PROGRESS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_SUCCESS;

class SnykExtendedLanguageClientTest extends LsBaseTest {
	private SnykExtendedLanguageClient cut;
	private Preferences pref;

	private ISnykToolView toolWindowMock;

	@BeforeEach
	protected void setUp() {
		super.setUp();
		pref = Preferences.getInstance();
		// we don't want the wizard to pop up, so we set a dummy token
		pref.store(Preferences.AUTH_TOKEN_KEY, "dummy");
		pref.store(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "false");
		toolWindowMock = mock(ISnykToolView.class);

	}

	@AfterEach
	protected void tearDown() {
		reset(toolWindowMock);
		super.tearDown();
	}

	@Test
	void testAddTrustedPathsAddsPathToPreferenceStore() {
		SnykTrustedFoldersParams param = new SnykTrustedFoldersParams();
		param.setTrustedFolders(new String[] { "trusted/path " });

		cut = new SnykExtendedLanguageClient();
		cut.addTrustedPaths(param);

		assertEquals("trusted/path", pref.getPref(Preferences.TRUSTED_FOLDERS, ""));
	}

	@Test
	void testAddTrustedPathsDeduplicatesAndTrims() {
		SnykTrustedFoldersParams param = new SnykTrustedFoldersParams();
		param.setTrustedFolders(new String[] { "trusted/path", "trusted/path", " trusted/path " });

		cut = new SnykExtendedLanguageClient();
		cut.addTrustedPaths(param);

		assertEquals("trusted/path", pref.getPref(Preferences.TRUSTED_FOLDERS, ""));
	}

	@Test
	void testHasAuthenticatedSavesTokenAndApiURL() {
		HasAuthenticatedParam param = new HasAuthenticatedParam();
		param.setApiUrl("https://abc.d/ef");
		param.setToken("testToken");

		cut = new SnykExtendedLanguageClient();
		cut.hasAuthenticated(param);

		assertEquals(param.getToken(), pref.getAuthToken());
		assertEquals(param.getApiUrl(), pref.getEndpoint());
	}

	@Test
	void testHasAuthenticatedCanHandleEmptyApiURL() {
		HasAuthenticatedParam param = new HasAuthenticatedParam();
		param.setToken("testToken");

		cut = new SnykExtendedLanguageClient();
		cut.hasAuthenticated(param);

		assertEquals(param.getToken(), pref.getAuthToken());
		assertEquals(Preferences.DEFAULT_ENDPOINT, pref.getEndpoint());
	}

	@Test
	void testHasAuthenticatedUpdatesPrefToEmptyToken() {
		HasAuthenticatedParam param = new HasAuthenticatedParam();
		param.setToken("");

		cut = new SnykExtendedLanguageClient();
		cut.hasAuthenticated(param);

		assertEquals(param.getToken(), pref.getAuthToken());
		assertEquals(Preferences.DEFAULT_ENDPOINT, pref.getEndpoint());
	}

//	@Test // the static mock does not work :(
	void testSendsPluginInstalledEventAndRefreshFeatureFlagOnFirstStart() {
		try (MockedStatic<TaskProcessor> mockedAnalyticsSender = mockStatic(TaskProcessor.class)) {
			var asMock = Mockito.mock(TaskProcessor.class);
			mockedAnalyticsSender.when(() -> TaskProcessor.getInstance()).thenReturn(asMock);

			cut = new SnykExtendedLanguageClient();

			verify(asMock, timeout(5000).times(2)).registerTask(any(), any());
			verifyNoMoreInteractions(asMock);
		}
	}

//	@Test // the static mock does not work :(
	void testDoesNotSendPluginInstalledEventOnSecondStart() {
		try (MockedStatic<TaskProcessor> mockedAnalyticsSender = mockStatic(TaskProcessor.class)) {
			var asMock = Mockito.mock(TaskProcessor.class);
			mockedAnalyticsSender.when(() -> TaskProcessor.getInstance()).thenReturn(asMock);
			pref.store(Preferences.ANALYTICS_PLUGIN_INSTALLED_SENT, "true");

			cut = new SnykExtendedLanguageClient();

			verify(asMock, timeout(5000).times(1)).registerTask(any(), any());
			verifyNoMoreInteractions(asMock);
		}
	}

	@Test
	void createProgressAddsTokenToProgressManager() {
		ProgressManager pmMock = mock(ProgressManager.class);
		cut = new SnykExtendedLanguageClient();
		cut.setProgressMgr(pmMock);
		var params = new WorkDoneProgressCreateParams();
		params.setToken("a");

		cut.createProgress(params);

		verify(pmMock).addProgress("a");
	}

	@Test
	void notifyProgress_EndProgressRemovesFromProgressManager() {
		ProgressManager pmMock = mock(ProgressManager.class);
		cut = new SnykExtendedLanguageClient();
		cut.setProgressMgr(pmMock);
		var params = new ProgressManager(cut).getEndProgressParam("a");
		params.setToken("a");

		cut.notifyProgress(params);

		verify(pmMock).removeProgress("a");
	}

	@Test
	void notifyProgress_BeginProgressDoesNotRemoveFromProgressManager() {
		ProgressManager pmMock = mock(ProgressManager.class);
		cut = new SnykExtendedLanguageClient();
		cut.setProgressMgr(pmMock);
		var progress = new WorkDoneProgressBegin();
		progress.setMessage("not important");
		Either<WorkDoneProgressNotification, Object> value = Either.forLeft(progress);
		Either<String, Integer> tokenEither = Either.forLeft("a");
		var progressParam = new ProgressParams(tokenEither, value);
		progressParam.setToken("a");

		cut.notifyProgress(progressParam);

		verifyNoInteractions(pmMock);
	}

	@Test
	void notifyProgress_ReportProgressDoesNotRemoveFromProgressManager() {
		ProgressManager pmMock = mock(ProgressManager.class);
		cut = new SnykExtendedLanguageClient();
		cut.setProgressMgr(pmMock);
		var progress = new WorkDoneProgressReport();
		progress.setMessage("not important");
		Either<WorkDoneProgressNotification, Object> value = Either.forLeft(progress);
		Either<String, Integer> tokenEither = Either.forLeft("a");
		var progressParam = new ProgressParams(tokenEither, value);
		progressParam.setToken("a");

		cut.notifyProgress(progressParam);

		verifyNoInteractions(pmMock);
	}

	@Test
	void snykConfigurationPersistsGlobalSettings() {
		var gson = new com.google.gson.Gson();
		String json = """
				{
					"settings": {
						"api_endpoint": {
							"value": "https://custom.snyk.io",
							"changed": true,
							"source": "cli",
							"originScope": "machine",
							"isLocked": false
						}
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("https://custom.snyk.io", pref.getPref(Preferences.ENDPOINT_KEY));
	}

	@Test
	void snykConfigurationStoresFolderConfigs() {
		var gson = new com.google.gson.Gson();
		var folderSettings = new FolderConfigSettings();
		FolderConfigSettings.setInstance(folderSettings);

		String json = """
				{
					"folderConfigs": [
						{
							"folderPath": "/tmp/project1",
							"settings": {
								"base_branch": {
									"value": "main"
								}
							}
						}
					]
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("main", folderSettings.getBaseBranch("/tmp/project1"));
	}

	@Test
	void snykConfigurationHandlesBothGlobalAndFolder() {
		var gson = new com.google.gson.Gson();
		var folderSettings = new FolderConfigSettings();
		FolderConfigSettings.setInstance(folderSettings);

		String json = """
				{
					"settings": {
						"api_endpoint": {
							"value": "https://both.snyk.io"
						}
					},
					"folderConfigs": [
						{
							"folderPath": "/tmp/project2",
							"settings": {
								"preferred_org": {
									"value": "my-org"
								}
							}
						}
					]
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("https://both.snyk.io", pref.getPref(Preferences.ENDPOINT_KEY));
		assertEquals("my-org", folderSettings.getPreferredOrg("/tmp/project2"));
	}

	@Test
	void snykConfigurationReplacesExistingFolderConfigs() {
		var gson = new com.google.gson.Gson();
		var folderSettings = new FolderConfigSettings();
		FolderConfigSettings.setInstance(folderSettings);

		// First notification
		String json1 = """
				{
					"folderConfigs": [
						{
							"folderPath": "/tmp/project1",
							"settings": { "base_branch": { "value": "main" } }
						}
					]
				}
				""";
		LspConfigurationParam param1 = gson.fromJson(json1, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param1);

		assertEquals("main", folderSettings.getBaseBranch("/tmp/project1"));

		// Second notification replaces all
		String json2 = """
				{
					"folderConfigs": [
						{
							"folderPath": "/tmp/project2",
							"settings": { "base_branch": { "value": "develop" } }
						}
					]
				}
				""";
		LspConfigurationParam param2 = gson.fromJson(json2, LspConfigurationParam.class);
		cut.snykConfiguration(param2);

		// project1 should be gone
		assertEquals("", folderSettings.getBaseBranch("/tmp/project1"));
		assertEquals("develop", folderSettings.getBaseBranch("/tmp/project2"));
	}

	@Test
	void snykConfigurationConvertsScanningModeAutomaticToBoolean() {
		var gson = new com.google.gson.Gson();
		String json = """
				{
					"settings": {
						"scan_automatic": {
							"value": true
						}
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("true", pref.getPref(Preferences.SCANNING_MODE_AUTOMATIC));
	}

	@Test
	void snykConfigurationConvertsScanningModeManualToBoolean() {
		var gson = new com.google.gson.Gson();
		String json = """
				{
					"settings": {
						"scan_automatic": {
							"value": false
						}
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("false", pref.getPref(Preferences.SCANNING_MODE_AUTOMATIC));
	}

	@Test
	void snykConfigurationConvertsJsonBooleanToString() {
		var gson = new com.google.gson.Gson();
		String json = """
				{
					"settings": {
						"snyk_code_enabled": {
							"value": true
						}
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("true", pref.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY));
	}

	@Test
	void snykConfigurationDoesNotMarkSettingsAsExplicitlyChanged() {
		var gson = new com.google.gson.Gson();
		String json = """
				{
					"settings": {
						"api_endpoint": {
							"value": "https://ls-pushed.snyk.io",
							"changed": true
						},
						"snyk_oss_enabled": {
							"value": "true",
							"changed": false
						},
						"cli_insecure": {
							"value": "false",
							"changed": false
						}
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		// Inbound settings use prefs.store(), not storeAndTrackChange,
		// so they should NOT be marked as explicitly changed by the user
		assertFalse(pref.isExplicitlyChanged(Preferences.ENDPOINT_KEY));
		assertFalse(pref.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
		assertFalse(pref.isExplicitlyChanged(Preferences.INSECURE_KEY));
	}

	@Test
	void snykConfigurationGlobalResetClearsValueAndExplicitChange() {
		var gson = new com.google.gson.Gson();

		// Simulate a pre-existing user:global override for an org-scope global key.
		pref.store(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "false");
		pref.markExplicitlyChanged(Preferences.ACTIVATE_SNYK_OPEN_SOURCE);
		assertTrue(pref.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));

		// LS Unset the global override and pushes back {value:null, changed:true}.
		String json = """
				{
					"settings": {
						"snyk_oss_enabled": {
							"value": null,
							"changed": true,
							"source": "cli",
							"originScope": "org"
						}
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		// Persisted override dropped (falls back to registry default "true").
		assertEquals("true", pref.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "true"));
		// Explicit-changed tracking cleared so it won't be re-asserted on next sync.
		assertFalse(pref.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
	}

	@Test
	void snykConfigurationNullValueWithoutChangedIsIgnored() {
		var gson = new com.google.gson.Gson();

		pref.store(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "false");
		pref.markExplicitlyChanged(Preferences.ACTIVATE_SNYK_OPEN_SOURCE);

		// value:null but changed:false (or absent) is NOT a reset — leave it alone.
		String json = """
				{
					"settings": {
						"snyk_oss_enabled": {
							"value": null,
							"changed": false
						}
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("false", pref.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "true"));
		assertTrue(pref.isExplicitlyChanged(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
	}

	@Test
	void snykConfigurationHandlesEmptyPayload() {
		var gson = new com.google.gson.Gson();
		LspConfigurationParam param = gson.fromJson("{}", LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		// Should not throw
		cut.snykConfiguration(param);
	}

	@Test
	void snykConfigurationNeverThrows() {
		cut = new SnykExtendedLanguageClient();
		// null param should not throw
		cut.snykConfiguration(null);
	}

	@Test
	void snykConfigurationPersistsSnykSecretsEnabled() {
		var gson = new com.google.gson.Gson();
		String json = """
				{
					"settings": {
						"snyk_secrets_enabled": {
							"value": true
						}
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("true", pref.getPref(Preferences.ACTIVATE_SNYK_SECRETS));
	}

	@Test
	void snykConfigurationPersistsAdditionalRegistryKeys() {
		var gson = new com.google.gson.Gson();
		String json = """
				{
					"settings": {
						"send_error_reports": { "value": "true" },
						"automatic_download": { "value": "false" },
						"proxy_insecure": { "value": "true" },
						"authentication_method": { "value": "token" },
						"scan_net_new": { "value": "true" }
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("true", pref.getPref(Preferences.SEND_ERROR_REPORTS));
		assertEquals("false", pref.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY));
		assertEquals("true", pref.getPref(Preferences.INSECURE_KEY));
		assertEquals("token", pref.getPref(Preferences.AUTHENTICATION_METHOD));
		assertEquals("true", pref.getPref(Preferences.ENABLE_DELTA));
	}

	@Test
	void snykConfigurationNeverPersistsInboundToken() {
		var gson = new com.google.gson.Gson();
		String json = """
				{
					"settings": {
						"token": { "value": "compromised-token" }
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		cut.snykConfiguration(param);

		assertEquals("dummy", pref.getPref(Preferences.AUTH_TOKEN_KEY));
	}

	@Test
	void snykConfigurationSkipsAlwaysFixedEntries() {
		var gson = new com.google.gson.Gson();
		// automatic_authentication and trust_enabled are always-fixed — inbound values ignored
		String json = """
				{
					"settings": {
						"automatic_authentication": { "value": "true" },
						"trust_enabled": { "value": "false" }
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		cut = new SnykExtendedLanguageClient();
		// should not throw; fixed entries have no prefKey so are skipped
		cut.snykConfiguration(param);
	}

	// showDocument with null toolView (test mode) must return false without NPE
	@Test
	void showDocument_withNullToolView_returnsFalse() throws Exception {
		cut = new SnykExtendedLanguageClient();
		// toolView intentionally not set; openToolView() is a no-op in test mode.

		ShowDocumentParams params = new ShowDocumentParams();
		params.setUri("snyk:///test?product=code&action=showInDetailPanel&issueId=test-issue-id");

		ShowDocumentResult result = cut.showDocument(params).get(5, TimeUnit.SECONDS);

		assertFalse(result.isSuccess());
	}

	// showDocument with toolView set calls selectTreeNode(issueId) and returns true
	@Test
	void showDocument_withToolViewSet_callsSelectTreeNodeAndReturnsTrue() throws Exception {
		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);

		ShowDocumentParams params = new ShowDocumentParams();
		params.setUri("snyk:///test?product=code&action=showInDetailPanel&issueId=test-issue-happy-path");

		ShowDocumentResult result = cut.showDocument(params).get(5, TimeUnit.SECONDS);

		assertTrue(result.isSuccess());
		verify(toolWindowMock).selectTreeNode("test-issue-happy-path", "code");
	}

	// file:// URIs must be handled by our own openFileInEclipse, not super.showDocument.
	// In test mode, openFileInEclipse returns false (isTest() guard) without delegating to
	// LSP4E's default which can route to the OS (Windsurf) for unknown file extensions.
	@Test
	void showDocument_withFileUri_returnsFalseInTestMode() throws Exception {
		cut = new SnykExtendedLanguageClient();

		ShowDocumentParams params = new ShowDocumentParams();
		params.setUri("file:///some/path/secret.key");

		ShowDocumentResult result = cut.showDocument(params).get(5, TimeUnit.SECONDS);

		// test mode: isTest() guard skips actual editor open, returns false without OS delegation
		assertFalse(result.isSuccess());
	}

	@Test
	void showDocument_withFileUriAndRange_returnsFalseInTestMode() throws Exception {
		cut = new SnykExtendedLanguageClient();

		ShowDocumentParams params = new ShowDocumentParams();
		params.setUri("file:///some/path/config.md");
		org.eclipse.lsp4j.Range range = new org.eclipse.lsp4j.Range(
				new org.eclipse.lsp4j.Position(2, 0),
				new org.eclipse.lsp4j.Position(2, 10));
		params.setSelection(range);

		ShowDocumentResult result = cut.showDocument(params).get(5, TimeUnit.SECONDS);

		assertFalse(result.isSuccess());
	}

	@Test
	void showDocument_withUnsupportedScheme_delegatesToSuper() {
		cut = new SnykExtendedLanguageClient();

		ShowDocumentParams params = new ShowDocumentParams();
		params.setUri("vscode://extension/something");

		// Unknown schemes are delegated to the LSP4E super implementation.
		// Verify a future is returned (delegation happened); don't assert the result
		// since super runs async UI operations unavailable in headless tests.
		assertDoesNotThrow(() -> cut.showDocument(params));
	}

	@Test
	void showDocument_withUnsupportedSnykAction_returnsFalse() throws Exception {
		cut = new SnykExtendedLanguageClient();

		ShowDocumentParams params = new ShowDocumentParams();
		params.setUri("snyk:///test?product=code&action=unknownAction&issueId=abc");

		ShowDocumentResult result = cut.showDocument(params).get(5, TimeUnit.SECONDS);

		assertFalse(result.isSuccess());
	}

	// showDocument with Secrets product URI calls selectTreeNode and returns true
	@Test
	void showDocument_withSecretsProduct_callsSelectTreeNodeAndReturnsTrue() throws Exception {
		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);

		ShowDocumentParams params = new ShowDocumentParams();
		params.setUri("snyk:///test?product=Snyk+Secrets&action=showInDetailPanel&issueId=secrets-issue-id");

		ShowDocumentResult result = cut.showDocument(params).get(5, TimeUnit.SECONDS);

		assertTrue(result.isSuccess());
		verify(toolWindowMock).selectTreeNode("secrets-issue-id", "Snyk Secrets");
	}

	@Test
	void snykScan_inProgress_callsRefreshBrowserWithParam() {
		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);

		SnykScanParam param = new SnykScanParam();
		param.setStatus(SCAN_STATE_IN_PROGRESS);

		cut.snykScan(param);

		verify(toolWindowMock).refreshBrowser(param);
	}

	@Test
	void snykScan_success_callsRefreshBrowserWithParam() {
		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);

		SnykScanParam param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);

		cut.snykScan(param);

		verify(toolWindowMock).refreshBrowser(param);
	}

	@Test
	void showDocument_withPathTraversalUri_returnsFalse() throws Exception {
		cut = new SnykExtendedLanguageClient();

		ShowDocumentParams params = new ShowDocumentParams();
		params.setUri("file:///some/../../../etc/passwd");

		ShowDocumentResult result = cut.showDocument(params).get(5, java.util.concurrent.TimeUnit.SECONDS);

		assertFalse(result.isSuccess(), "Path traversal URI must be rejected");
	}

	@Test
	void cancelLogin_withNoActiveLogin_doesNotThrow() {
		cut = new SnykExtendedLanguageClient();
		// cancelLogin() when authCompleteFuture is null must not throw
		assertDoesNotThrow(() -> cut.cancelLogin());
	}

	@Test
	void cancelLogin_cancelsAuthFuture() throws Exception {
		cut = new SnykExtendedLanguageClient();

		// Set authCompleteFuture directly via reflection to avoid hitting executeCommand/LS
		java.util.concurrent.CompletableFuture<Void> future = new java.util.concurrent.CompletableFuture<>();
		setAuthCompleteFuture(cut, future);

		assertFalse(future.isDone(), "Future must not be done before cancel");
		cut.cancelLogin();
		assertTrue(future.isCancelled(), "Future must be cancelled after cancelLogin()");
	}

	@Test
	void hasAuthenticated_completesAuthFuture() throws Exception {
		cut = new SnykExtendedLanguageClient();

		// Set authCompleteFuture directly via reflection to avoid hitting executeCommand/LS
		java.util.concurrent.CompletableFuture<Void> future = new java.util.concurrent.CompletableFuture<>();
		setAuthCompleteFuture(cut, future);

		assertFalse(future.isDone());

		HasAuthenticatedParam param = new HasAuthenticatedParam();
		param.setToken("test-token");
		param.setApiUrl("https://api.snyk.io");
		cut.hasAuthenticated(param);

		assertTrue(future.isDone(), "authFuture must complete when hasAuthenticated fires");
		assertFalse(future.isCompletedExceptionally());
	}

	@SuppressWarnings("unchecked")
	private static void setAuthCompleteFuture(SnykExtendedLanguageClient lc,
			java.util.concurrent.CompletableFuture<Void> future) throws Exception {
		java.lang.reflect.Field f = SnykExtendedLanguageClient.class.getDeclaredField("authCompleteFuture");
		f.setAccessible(true);
		((java.util.concurrent.atomic.AtomicReference<java.util.concurrent.CompletableFuture<Void>>) f.get(lc)).set(future);
	}

	@Test
	void snykConfigurationReloadsOpenSettingsPage() {
		var gson = new com.google.gson.Gson();
		String json = """
				{
					"settings": {
						"api_endpoint": {
							"value": "https://reload-test.snyk.io"
						}
					}
				}
				""";
		LspConfigurationParam param = gson.fromJson(json, LspConfigurationParam.class);

		try (MockedStatic<HTMLSettingsPreferencePage> mockedPage = mockStatic(HTMLSettingsPreferencePage.class)) {
			cut = new SnykExtendedLanguageClient();
			cut.snykConfiguration(param);

			mockedPage.verify(() -> HTMLSettingsPreferencePage.reloadIfOpen(), Mockito.times(1));
		}
	}

	@Test
	void snykConfigurationReloadsEvenWithEmptyPayload() {
		var gson = new com.google.gson.Gson();
		LspConfigurationParam param = gson.fromJson("{}", LspConfigurationParam.class);

		try (MockedStatic<HTMLSettingsPreferencePage> mockedPage = mockStatic(HTMLSettingsPreferencePage.class)) {
			cut = new SnykExtendedLanguageClient();
			assertDoesNotThrow(() -> cut.snykConfiguration(param));

			mockedPage.verify(() -> HTMLSettingsPreferencePage.reloadIfOpen(), Mockito.times(1));
		}
	}
}
