package io.snyk.languageserver.protocolextension;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_CODE;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_OSS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_TO_DISPLAYED;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_IN_PROGRESS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_SUCCESS;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.mockito.ArgumentMatchers.any;
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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.lsp4j.ProgressParams;
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
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.InfoTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.ProductTreeNode;
import io.snyk.languageserver.IssueCacheHolder;
import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.ScanInProgressKey;
import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.SnykIssueCache;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;
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
		var expectedNodes = List.of("✋ 1 open issue & 0 ignored issues", "⚡️ 1 open issue is fixable automatically.");
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
}
