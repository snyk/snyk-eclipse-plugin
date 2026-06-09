package io.snyk.languageserver.protocolextension;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DIAGNOSTIC_SOURCE_SNYK_CODE;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DIAGNOSTIC_SOURCE_SNYK_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DIAGNOSTIC_SOURCE_SNYK_OSS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_CODE;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_OSS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_TO_DISPLAYED;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_IN_PROGRESS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_SUCCESS;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.after;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.ShowDocumentParams;
import org.eclipse.lsp4j.ShowDocumentResult;
import org.eclipse.lsp4j.WorkDoneProgressBegin;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.WorkDoneProgressNotification;
import org.eclipse.lsp4j.WorkDoneProgressReport;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.instancio.Instancio;
import org.instancio.Select;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import io.snyk.eclipse.plugin.analytics.TaskProcessor;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.InfoTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.ProductTreeNode;
import io.snyk.languageserver.IssueCacheHolder;
import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.ScanInProgressKey;
import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.SnykIssueCache;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.LspConfigurationParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.AdditionalData;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

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
	void testSnykScanUpdatesRootNodeStatus() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_IN_PROGRESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");
		ProductTreeNode productNode = new ProductTreeNode(DISPLAYED_CODE_SECURITY);
		when(toolWindowMock.getProductNode(DISPLAYED_CODE_SECURITY, param.getFolderPath())).thenReturn(productNode);
		pref.store(Preferences.ACTIVATE_SNYK_CODE_SECURITY, "true");

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		// expect "scanning..."
		verify(toolWindowMock).getProductNode(DISPLAYED_CODE_SECURITY, param.getFolderPath());
		verify(toolWindowMock).setNodeText(productNode, ISnykToolView.NODE_TEXT_SCANNING);
	}

	void disableCCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "false");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_NoIssuesFound_IAC_NoCCI() {
		disableCCI();
		var scanProduct = SCAN_PARAMS_IAC;
		int totalIssueCount = 0;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of(ISnykToolView.CONGRATS_NO_ISSUES_FOUND);
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_NoIssuesFound_IAC_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");
		var scanProduct = SCAN_PARAMS_IAC;
		int totalIssueCount = 0;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of(ISnykToolView.CONGRATS_NO_ISSUES_FOUND);
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_NoIssuesFound_Code_NoCCI() {
		disableCCI();
		var scanProduct = SCAN_PARAMS_CODE;
		int totalIssueCount = 0;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of(ISnykToolView.CONGRATS_NO_ISSUES_FOUND);
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_NoIssuesFound_Code_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");
		var scanProduct = SCAN_PARAMS_CODE;
		int totalIssueCount = 0;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of(ISnykToolView.CONGRATS_NO_ISSUES_FOUND);
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFoundButNothingFixable_IAC_NoCCI() {
		disableCCI();
		var scanProduct = SCAN_PARAMS_IAC;
		int totalIssueCount = 3;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of("✋ 3 issues", ISnykToolView.NO_FIXABLE_ISSUES);
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFoundButNothingFixable_IAC_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");
		var scanProduct = SCAN_PARAMS_IAC;
		int totalIssueCount = 3;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of("✋ 3 issues", ISnykToolView.NO_FIXABLE_ISSUES);
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFoundButNothingFixableAndHidingIgnored_Code_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "false");
		var scanProduct = SCAN_PARAMS_CODE;
		int totalIssueCount = 3;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of(
				"✋ 3 open issues",
				ISnykToolView.NO_FIXABLE_ISSUES);
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_OneFixableIssueFound_IAC_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");
		var scanProduct = SCAN_PARAMS_IAC;
		int totalIssueCount = 1;
		int fixableIssueCount = 1;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of("✋ 1 issue", "⚡️ 1 issue is fixable automatically.");
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_OneFixableIssueFound_Code_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");
		var scanProduct = SCAN_PARAMS_CODE;
		int totalIssueCount = 1;
		int fixableIssueCount = 1;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of("✋ 1 open issue", "⚡️ 1 open issue is fixable automatically.");
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_MultipleFixableIssuesFoundAndHidingIgnored_Code_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "false");
		var scanProduct = SCAN_PARAMS_CODE;
		int totalIssueCount = 4;
		int fixableIssueCount = 2;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of("✋ 4 open issues", "⚡️ 2 open issues are fixable automatically.");
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFoundAndOneIgnored_Code_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");
		var scanProduct = SCAN_PARAMS_CODE;
		int totalIssueCount = 4;
		int fixableIssueCount = 2;
		int ignoredIssueCount = 1;
		var expectedNodes = List.of("✋ 3 open issues & 1 ignored issue", "⚡️ 2 open issues are fixable automatically.");
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_HidingOpen_IAC_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "false");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");
		var scanProduct = SCAN_PARAMS_IAC;
		int totalIssueCount = 0;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of(ISnykToolView.OPEN_ISSUES_ARE_DISABLED, ISnykToolView.OPEN_ISSUES_FILTERED_BUT_AVAILABLE);
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_MultipleIgnoredIssuesFoundAndHidingOpen_Code_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "false");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");
		var scanProduct = SCAN_PARAMS_CODE;
		int totalIssueCount = 4;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 4;
		var expectedNodes = List.of("✋ 4 ignored issues, open issues are disabled");
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_HidingOpenAndIgnored_Code_CCI() {
		pref.store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "false");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "false");
		var scanProduct = SCAN_PARAMS_CODE;
		int totalIssueCount = 0;
		int fixableIssueCount = 0;
		int ignoredIssueCount = 0;
		var expectedNodes = List.of(ISnykToolView.OPEN_AND_IGNORED_ISSUES_ARE_DISABLED,
				ISnykToolView.ALL_ISSUES_FILTERED_BUT_AVAILABLE);
		runInfoNodeTest(scanProduct, totalIssueCount, fixableIssueCount, ignoredIssueCount, expectedNodes);
	}

	private void runInfoNodeTest(String scanProduct, int totalIssueCount, int fixableCount, int ignoredCount,
			List<String> expectedNodes) {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(scanProduct);
		param.setFolderPath("a/b/c");

		var productNodes = setupProductNodes(param);

		pref.store(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, scanProduct == SCAN_PARAMS_OSS ? "true" : "false");
		pref.store(Preferences.ACTIVATE_SNYK_CODE_SECURITY, scanProduct == SCAN_PARAMS_CODE ? "true" : "false");
		pref.store(Preferences.ACTIVATE_SNYK_IAC, scanProduct == SCAN_PARAMS_IAC ? "true" : "false");

		var infoNodeCaptor = ArgumentCaptor.forClass(InfoTreeNode.class);
		var parentCaptor = ArgumentCaptor.forClass(ProductTreeNode.class);

		Path folderPath = Paths.get(param.getFolderPath());
		var dummyFilePath = Paths.get(param.getFolderPath(), "d").toString();

		Set<Issue> issues = getIssues(totalIssueCount, fixableCount, ignoredCount);

		var cache = new SnykIssueCache(folderPath);
		IssueCacheHolder.getInstance().addCacheForTest(cache);
		switch (scanProduct) {
		case SCAN_PARAMS_OSS:
			cache.addOssIssues(dummyFilePath, issues);
			break;
		case SCAN_PARAMS_CODE:
			cache.addCodeIssues(dummyFilePath, issues);
			break;
		case SCAN_PARAMS_IAC:
			cache.addIacIssues(dummyFilePath, issues);
			break;
		}
		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		for (ProductTreeNode node : productNodes) {
			verify(toolWindowMock).getProductNode(node.getProduct(), param.getFolderPath());
		}

		verify(toolWindowMock, atLeastOnce()).addInfoNode(parentCaptor.capture(), infoNodeCaptor.capture());

		List<Object> actualNodes = infoNodeCaptor.getAllValues().stream().map(InfoTreeNode::getValue)
				.collect(Collectors.toList());

		assertIterableEquals(expectedNodes, actualNodes);
	}

	private Set<Issue> getIssues(int totalIssueCount, int fixableCount, int ignoredCount) {
		Set<Issue> issues = new HashSet<Issue>(totalIssueCount);
		int setAsFixable = 0;
		int setAsIgnored = 0;
		for (int i = 0; i < totalIssueCount; i++) {
			boolean fixable = fixableCount > setAsFixable++;
			boolean ignored = ignoredCount > setAsIgnored++;
			var additionalData = Instancio.of(AdditionalData.class)
					.set(Select.field(AdditionalData::isUpgradable), fixable)
					.set(Select.field(AdditionalData::hasAIFix), fixable).create();

			Issue issue = Instancio.of(Issue.class).set(Select.field(Issue::additionalData), additionalData)
					.set(Select.field(Issue::isIgnored), ignored).create();
			issues.add(issue);
		}
		return issues;
	}

	private HashSet<ProductTreeNode> setupProductNodes(SnykScanParam param) {
		String displayProduct = SCAN_PARAMS_TO_DISPLAYED.get(param.getProduct());
		var productNodes = new HashSet<ProductTreeNode>();
		ProductTreeNode node = new ProductTreeNode(displayProduct);
		productNodes.add(node);
		when(toolWindowMock.getProductNode(displayProduct, param.getFolderPath())).thenReturn(node);
		return productNodes;
	}

	@Test
	void testSnykScanAddsToScanStateHashMap() {
		var scanState = ScanState.getInstance();
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_IN_PROGRESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		var expectedKey = new ScanInProgressKey("a/b/c", SCAN_PARAMS_CODE);
		var actualState = scanState.isScanInProgress(expectedKey);
		assertEquals(true, actualState);

		param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		expectedKey = new ScanInProgressKey("a/b/c", SCAN_PARAMS_CODE);
		actualState = scanState.isScanInProgress(expectedKey);
		assertEquals(false, actualState);
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
	void testGetTotal() throws IOException {
		cut = new SnykExtendedLanguageClient();
		ProductTreeNode productTreeNode = new ProductTreeNode(DISPLAYED_CODE_SECURITY);
		SnykIssueCache issueCache = new SnykIssueCache(Path.of("a/b"));
		var issues = new HashSet<Issue>();
		var highIssue = Instancio.of(Issue.class).set(Select.field(Issue::additionalData), getSecurityIssue())
				.set(Select.field(Issue::severity), "high").create();
		var mediumIssue = Instancio.of(Issue.class).set(Select.field(Issue::additionalData), getSecurityIssue())
				.set(Select.field(Issue::severity), "medium").create();
		var lowIssue = Instancio.of(Issue.class).set(Select.field(Issue::additionalData), getSecurityIssue())
				.set(Select.field(Issue::severity), "low").create();
		issues.addAll(Set.of(highIssue, mediumIssue, lowIssue));

		issueCache.addCodeIssues("a/b/c", issues);

		var actual = cut.getCountsSuffix(productTreeNode, issueCache);

		assertEquals("3 unique vulnerabilities: 0 critical, 1 high, 1 medium, 1 low", actual);
	}

	private AdditionalData getSecurityIssue() {
		return Instancio.of(AdditionalData.class).create();
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

	// Regression: showDocument must not NPE when toolView is null after openToolView()
	// is a no-op (test mode). Issue IS in cache so the code enters the supplyAsync
	// block and reaches the null guard at toolView == null.
	@Test
	void showDocument_withNullToolViewAndIssueInCache_returnsFalseWithoutNpe() throws Exception {
		Path projectPath = Paths.get("/test/project-null-toolview");
		SnykIssueCache cache = new SnykIssueCache(projectPath);
		String issueId = "test-issue-for-null-guard";
		Issue issue = Instancio.of(Issue.class).set(Select.field(Issue::id), issueId).create();
		cache.addCodeIssues("/test/project-null-toolview/Test.java", Set.of(issue));
		IssueCacheHolder.getInstance().addCacheForTest(cache);

		IProject mockProject = mock(IProject.class);
		try (MockedStatic<ResourceUtils> mockedResourceUtils = mockStatic(ResourceUtils.class)) {
			mockedResourceUtils.when(ResourceUtils::getAccessibleTopLevelProjects)
					.thenReturn(List.of(mockProject));
			mockedResourceUtils.when(() -> ResourceUtils.getFullPath(mockProject))
					.thenReturn(projectPath);

			cut = new SnykExtendedLanguageClient();
			// toolView intentionally not set; openToolView() is a no-op in test mode.
			// Without the null guard this would NPE on this.toolView.selectTreeNode().

			ShowDocumentParams params = new ShowDocumentParams();
			params.setUri("snyk:///test?product=code&action=showInDetailPanel&issueId=" + issueId);

			ShowDocumentResult result = cut.showDocument(params).get(5, TimeUnit.SECONDS);

			assertFalse(result.isSuccess());
		}
	}

	// Regression: showDocument with toolView set and issue in cache must call
	// selectTreeNode and return true (null guard must not block the happy path).
	@Test
	void showDocument_withToolViewSetAndIssueInCache_callsSelectTreeNodeAndReturnsTrue() throws Exception {
		Path projectPath = Paths.get("/test/project-with-toolview");
		SnykIssueCache cache = new SnykIssueCache(projectPath);
		String issueId = "test-issue-for-happy-path";
		Issue issue = Instancio.of(Issue.class).set(Select.field(Issue::id), issueId).create();
		cache.addCodeIssues("/test/project-with-toolview/Test.java", Set.of(issue));
		IssueCacheHolder.getInstance().addCacheForTest(cache);

		IProject mockProject = mock(IProject.class);
		try (MockedStatic<ResourceUtils> mockedResourceUtils = mockStatic(ResourceUtils.class)) {
			mockedResourceUtils.when(ResourceUtils::getAccessibleTopLevelProjects)
					.thenReturn(List.of(mockProject));
			mockedResourceUtils.when(() -> ResourceUtils.getFullPath(mockProject))
					.thenReturn(projectPath);

			cut = new SnykExtendedLanguageClient();
			cut.setToolWindow(toolWindowMock);

			ShowDocumentParams params = new ShowDocumentParams();
			params.setUri("snyk:///test?product=code&action=showInDetailPanel&issueId=" + issueId);

			ShowDocumentResult result = cut.showDocument(params).get(5, TimeUnit.SECONDS);

			assertTrue(result.isSuccess());
			verify(toolWindowMock).selectTreeNode(any(), any());
		}
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

	// Fix 2: normalizeProductCodename must map SCAN_PARAMS_* constants to DIAGNOSTIC_SOURCE_* constants
	@Test
	void normalizeProductCodename_oss_returnsDiagnosticSourceSnykOss() {
		assertEquals(DIAGNOSTIC_SOURCE_SNYK_OSS, SnykExtendedLanguageClient.normalizeProductCodename(SCAN_PARAMS_OSS));
	}

	@Test
	void normalizeProductCodename_code_returnsDiagnosticSourceSnykCode() {
		assertEquals(DIAGNOSTIC_SOURCE_SNYK_CODE, SnykExtendedLanguageClient.normalizeProductCodename(SCAN_PARAMS_CODE));
	}

	@Test
	void normalizeProductCodename_iac_returnsDiagnosticSourceSnykIac() {
		assertEquals(DIAGNOSTIC_SOURCE_SNYK_IAC, SnykExtendedLanguageClient.normalizeProductCodename(SCAN_PARAMS_IAC));
	}

	@Test
	void normalizeProductCodename_unknown_returnsInputUnchanged() {
		assertEquals("custom", SnykExtendedLanguageClient.normalizeProductCodename("custom"));
	}

	@Test
	void normalizeProductCodename_null_returnsNull() {
		assertNull(SnykExtendedLanguageClient.normalizeProductCodename(null));
	}
}
