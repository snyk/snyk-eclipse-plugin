package io.snyk.languageserver.protocolextension;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DIAGNOSTIC_SOURCE_SNYK_CODE;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_QUALITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_CODE;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_TO_DISPLAYED;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_IN_PROGRESS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_SUCCESS;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.CONGRATS_NO_ISSUES_FOUND;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.getPlural;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.WorkDoneProgressEnd;
import org.eclipse.lsp4j.WorkDoneProgressNotification;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.instancio.Instancio;
import org.instancio.Select;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.snyk.eclipse.plugin.analytics.AnalyticsEvent;
import io.snyk.eclipse.plugin.analytics.AnalyticsSender;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.InfoTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.ProductTreeNode;
import io.snyk.languageserver.IssueCacheHolder;
import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.ScanInProgressKey;
import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.protocolextension.ProgressManager.SnykBackgroundJob;
import io.snyk.languageserver.protocolextension.messageObjects.Diagnostic316;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.PublishDiagnostics316Param;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.AdditionalData;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

@SuppressWarnings("restriction")
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

	@Test
	void testSendsPluginInstalledEventOnFirstStart() {
		try (MockedStatic<AnalyticsSender> mockedAnalyticsSender = mockStatic(AnalyticsSender.class)) {
			var asMock = Mockito.mock(AnalyticsSender.class);
			mockedAnalyticsSender.when(() -> AnalyticsSender.getInstance()).thenReturn(asMock);

			cut = new SnykExtendedLanguageClient();

			var captor = ArgumentCaptor.forClass(AnalyticsEvent.class);
			verify(asMock).logEvent(captor.capture(), any());
			verifyNoMoreInteractions(asMock);

			assertEquals("plugin installed", captor.getValue().getInteractionType());
		}
	}

	@Test
	void testDoesNotSendPluginInstalledEventOnSecondStart() {
		try (MockedStatic<AnalyticsSender> mockedAnalyticsSender = mockStatic(AnalyticsSender.class)) {
			var asMock = Mockito.mock(AnalyticsSender.class);
			mockedAnalyticsSender.when(() -> AnalyticsSender.getInstance()).thenReturn(asMock);
			pref.store(Preferences.ANALYTICS_PLUGIN_INSTALLED_SENT, "true");

			cut = new SnykExtendedLanguageClient();

			verifyNoMoreInteractions(asMock);
		}
	}

	@Test
	void testPublishDiagnosticsShouldChangeCache() {
		// Test Add to cache
		String folderPath = "/a/b";
		var issueCache = IssueCacheHolder.getInstance().getCacheInstance(folderPath);

		var uri = "file://" + folderPath + "/c/";
		var filePath = LSPEclipseUtils.fromUri(URI.create(uri)).getAbsolutePath();

		var param = new PublishDiagnostics316Param();
		param.setUri(uri);
		var diagnostic = new Diagnostic316();
		diagnostic.setSource(DIAGNOSTIC_SOURCE_SNYK_CODE);

		var issue = Instancio.of(Issue.class).create();
		diagnostic.setData(issue);
		param.setDiagnostics(new Diagnostic316[] { diagnostic });
		cut = new SnykExtendedLanguageClient();
		var future = cut.publishDiagnostics316(param);
		try {
			future.get(5, TimeUnit.SECONDS);
		} catch (Exception ex) {

		}

		Collection<Issue> actualIssueList = null;
		if (issue.additionalData().isSecurityType()) {
			actualIssueList = issueCache.getCodeSecurityIssuesForPath(filePath);
		} else {
			actualIssueList = issueCache.getCodeQualityIssuesForPath(filePath);
		}
		assertEquals(1, actualIssueList.size());
		assertEquals(issue.id(), actualIssueList.stream().findFirst().get().id());

		// Test remove from cache
		param = new PublishDiagnostics316Param();
		param.setUri(uri);

		cut = new SnykExtendedLanguageClient();
		future = cut.publishDiagnostics316(param);
		try {
			future.get(5, TimeUnit.SECONDS);
		} catch (Exception ex) {

		}
		assertEquals(true, issueCache.getCodeSecurityIssuesForPath(filePath).isEmpty());
	}

	@Test
	void testSnykScanUpdatesRootNodeStatus() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_IN_PROGRESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");
		ProductTreeNode productNode = new ProductTreeNode(DISPLAYED_CODE_SECURITY);
		when(toolWindowMock.getProductNode(DISPLAYED_CODE_SECURITY, param.getFolderPath())).thenReturn(productNode);

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		// expect "scanning..."
		verify(toolWindowMock).getProductNode(DISPLAYED_CODE_SECURITY, param.getFolderPath());
		verify(toolWindowMock).setNodeText(productNode, "Scanning...");
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_NoIssuesFound() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");

		int issueCount = 0;
		String expectedFirstInfoNode = CONGRATS_NO_ISSUES_FOUND;
		String expectedSecondInfoNode = CONGRATS_NO_ISSUES_FOUND;

		runInfoNodeTest(param, issueCount, 0, 0, 2, expectedFirstInfoNode, expectedSecondInfoNode, null);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_NoIssuesFound_IAC() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(SCAN_PARAMS_IAC);
		param.setFolderPath("a/b/c");

		int issueCount = 0;
		String expectedFirstInfoNode = CONGRATS_NO_ISSUES_FOUND;

		runInfoNodeTest(param, issueCount, 0, 0, 1, expectedFirstInfoNode, null, null);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFound_nothingFixable() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");

		int issueCount = 3;
		String expectedFirstInfoNode = "✋ " + issueCount + " issue" + getPlural(issueCount) + " found by Snyk";
		String expectedSecondInfoNode = ISnykToolView.NO_FIXABLE_ISSUES;
		String expectedThirdInfoNode = CONGRATS_NO_ISSUES_FOUND;

		runInfoNodeTest(param, issueCount, 0, 0, 3, expectedFirstInfoNode, expectedSecondInfoNode,
				expectedThirdInfoNode);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFound_singleFixable() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");
		String expectedFirstInfoNode = "✋ 1 issue found by Snyk";
		String expectedSecondInfoNode = "⚡️ 1 issue can be fixed automatically";
		String expectedThirdInfoNode = CONGRATS_NO_ISSUES_FOUND;

		runInfoNodeTest(param, 1, 1, 0, 3, expectedFirstInfoNode, expectedSecondInfoNode, expectedThirdInfoNode);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFound_multipleFixable() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");
		String expectedFirstInfoNode = "✋ 4 issues found by Snyk";
		String expectedSecondInfoNode = "⚡️ 2 issues can be fixed automatically";
		String expectedThirdInfoNode = CONGRATS_NO_ISSUES_FOUND;

		runInfoNodeTest(param, 4, 2, 0, 3, expectedFirstInfoNode, expectedSecondInfoNode, expectedThirdInfoNode);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFound_oneIgnored() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");
		String expectedFirstInfoNode = "✋ 4 issues found by Snyk, 1 ignored";
		String expectedSecondInfoNode = "⚡️ 2 issues can be fixed automatically";
		String expectedThirdInfoNode = CONGRATS_NO_ISSUES_FOUND;

		runInfoNodeTest(param, 4, 2, 1, 3, expectedFirstInfoNode, expectedSecondInfoNode, expectedThirdInfoNode);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFound_onlyIgnoredDisplayed() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");

		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "true");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "false");

		String expectedFirstInfoNode = "✋ 4 issues found by Snyk, 4 ignored";
		String expectedSecondInfoNode = "⚡️ 2 issues can be fixed automatically";
		String expectedThirdInfoNode = "Adjust your Issue View Options to see ignored issues.";

		runInfoNodeTest(param, 4, 2, 4, 4, expectedFirstInfoNode, expectedSecondInfoNode, expectedThirdInfoNode);
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFound_onlyOpenDisplayed() {
		var param = new SnykScanParam();
		param.setStatus(SCAN_STATE_SUCCESS);
		param.setProduct(SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");

		pref.store(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, "false");
		pref.store(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, "true");

		String expectedFirstInfoNode = "✋ 4 issues found by Snyk";
		String expectedSecondInfoNode = "⚡️ 2 issues can be fixed automatically";
		String expectedThirdInfoNode = "Adjust your Issue View Options to see open issues.";

		runInfoNodeTest(param, 4, 2, 0, 4, expectedFirstInfoNode, expectedSecondInfoNode, expectedThirdInfoNode);
	}

	@Test
	void cancelAllProgresses_WithNullProgressManager_ShouldDoNothing() {
		MockitoAnnotations.openMocks(this);
		cut = new SnykExtendedLanguageClient();

		cut.setProgressMgr(null);
		cut.cancelAllProgresses();
		// No exceptions should be thrown
	}

	@Test
	void cancelAllProgresses_WithMultipleProgresses_ShouldNotifyProgressForEach() {
	    MockitoAnnotations.openMocks(this);
	    cut = new SnykExtendedLanguageClient();
	    
	    ConcurrentHashMap<String, Pair<SnykBackgroundJob, IProgressMonitor>> progresses = new ConcurrentHashMap<>();
	    progresses.put("token1", Pair.of(mock(SnykBackgroundJob.class), mock(IProgressMonitor.class)));
	    progresses.put("token2", Pair.of(mock(SnykBackgroundJob.class), mock(IProgressMonitor.class)));
	    WorkDoneProgressCreateParams workDoneProgressStart1 = new WorkDoneProgressCreateParams();
	    workDoneProgressStart1.setToken("token1");
	    cut.createProgress(workDoneProgressStart1);
	    WorkDoneProgressCreateParams workDoneProgressStart2 = new WorkDoneProgressCreateParams();
	    workDoneProgressStart2.setToken("token2");
	    cut.createProgress(workDoneProgressStart2);
	    assertEquals(cut.getProgressMgr().progresses.size(), 2);

	    cut.cancelAllProgresses();

	    assertEquals(cut.getProgressMgr().progresses.size(), 0);
	}

	private void runInfoNodeTest(SnykScanParam param, int issueCount, int fixableCount, int ignoredCount,
			int expectedInfoNodeUpdateCount, String expectedFirstInfoNode, String expectedSecondInfoNode,
			String expectedThirdInfoNode) {

		var productNodes = setupProductNodes(param);

		var infoNodeCaptor = ArgumentCaptor.forClass(InfoTreeNode.class);
		var parentCaptor = ArgumentCaptor.forClass(ProductTreeNode.class);

		var dummyFilePath = Paths.get(param.getFolderPath(), "d").toString();

		Set<Issue> issues = getIssues(issueCount, fixableCount, ignoredCount);

		var cache = IssueCacheHolder.getInstance().getCacheInstance(param.getFolderPath());
		cache.addCodeIssues(dummyFilePath, issues);
		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		for (ProductTreeNode node : productNodes) {
			verify(toolWindowMock).getProductNode(node.getProduct(), param.getFolderPath());
		}

		verify(toolWindowMock, times(expectedInfoNodeUpdateCount)).addInfoNode(any(), any());

		// if no issues found, we don't need to display the "no fixable issues" node
		verify(toolWindowMock, times(expectedInfoNodeUpdateCount)).addInfoNode(parentCaptor.capture(),
				infoNodeCaptor.capture());

		assertEquals(expectedInfoNodeUpdateCount, infoNodeCaptor.getAllValues().size());
		assertEquals(expectedFirstInfoNode, infoNodeCaptor.getAllValues().get(0).getValue());
		if (expectedInfoNodeUpdateCount > 1) {
			assertEquals(expectedSecondInfoNode, infoNodeCaptor.getAllValues().get(1).getValue());
			if (expectedInfoNodeUpdateCount > 2) {
				assertEquals(expectedThirdInfoNode, infoNodeCaptor.getAllValues().get(2).getValue());
			}
		}
	}

	private Set<Issue> getIssues(int issueCount, int fixableCount, int ignoredCount) {
		Set<Issue> issues = new HashSet<Issue>(issueCount);
		int setAsFixable = 0;
		int setAsIgnored = 0;
		for (int i = 0; i < issueCount; i++) {
			boolean fixable = fixableCount > setAsFixable++;
			boolean ignored = ignoredCount > setAsIgnored++;
			var additionalData = Instancio.of(AdditionalData.class)
					.set(Select.field(AdditionalData::isSecurityType), true)
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
		boolean notSnykCode = displayProduct != null;
		if (notSnykCode) {
			ProductTreeNode node = new ProductTreeNode(displayProduct);
			productNodes.add(node);
			when(toolWindowMock.getProductNode(displayProduct, param.getFolderPath())).thenReturn(node);
		} else {
			ProductTreeNode codeSecurityProductNode = new ProductTreeNode(DISPLAYED_CODE_SECURITY);
			ProductTreeNode codeQualityProductNode = new ProductTreeNode(DISPLAYED_CODE_QUALITY);
			productNodes.add(codeSecurityProductNode);
			productNodes.add(codeQualityProductNode);
			when(toolWindowMock.getProductNode(DISPLAYED_CODE_SECURITY, param.getFolderPath()))
					.thenReturn(codeSecurityProductNode);
			when(toolWindowMock.getProductNode(DISPLAYED_CODE_QUALITY, param.getFolderPath()))
					.thenReturn(codeQualityProductNode);
		}
		return productNodes;
	}

	@Test
	void testFileNodesAndIssueNodesAreAdded() {
		throw new NotImplementedException();
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
}
