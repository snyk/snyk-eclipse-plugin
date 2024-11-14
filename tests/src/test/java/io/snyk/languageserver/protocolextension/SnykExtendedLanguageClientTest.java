package io.snyk.languageserver.protocolextension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
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
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.instancio.Instancio;
import org.instancio.Select;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.analytics.AnalyticsEvent;
import io.snyk.eclipse.plugin.analytics.AnalyticsSender;
import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.ScanInProgressKey;
import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.SnykIssueCache;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;
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
		var issueCache = new SnykIssueCache();
		var uri = "file:///a/b/c/";
		var filePath = LSPEclipseUtils.fromUri(URI.create(uri)).getAbsolutePath();

		var param = new PublishDiagnosticsParams();
		param.setUri(uri);
		var diagnostic = new Diagnostic();
		diagnostic.setSource("Snyk Code");

		var issue = Instancio.of(Issue.class).create();
		ObjectMapper objectMapper = new ObjectMapper();
		String res;
		try {
			res = objectMapper.writeValueAsString(issue);
		} catch (Exception e) {
			res = "{}";
		}
		diagnostic.setData(res);
		param.setDiagnostics(List.of(diagnostic));
		cut = new SnykExtendedLanguageClient();
		cut.setIssueCache(issueCache);
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
		param = new PublishDiagnosticsParams();
		param.setUri(uri);

		cut = new SnykExtendedLanguageClient();
		cut.setIssueCache(issueCache);
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
		param.setStatus("inProgress");
		param.setProduct(ProductConstants.SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");
		TreeNode productNode = new TreeNode("Code Issues");
		when(toolWindowMock.getProductNode(param.getProduct())).thenReturn(productNode);

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		// expect "scanning..."
		verify(toolWindowMock).getProductNode(param.getProduct());
		verify(toolWindowMock).setNodeText(productNode, "Scanning...");
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_NoIssuesFound() {
		var param = new SnykScanParam();
		param.setStatus("success");
		param.setProduct(ProductConstants.SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");
		TreeNode codeSecurityProductNode = new TreeNode(ProductConstants.DISPLAYED_CODE_SECURITY);
		TreeNode codeQualityProductNode = new TreeNode(ProductConstants.DISPLAYED_CODE_QUALITY);

		when(toolWindowMock.getProductNode(ProductConstants.DISPLAYED_CODE_SECURITY))
				.thenReturn(codeSecurityProductNode);
		when(toolWindowMock.getProductNode(ProductConstants.DISPLAYED_CODE_QUALITY)).thenReturn(codeQualityProductNode);
		var infoNodeCaptor = ArgumentCaptor.forClass(TreeNode.class);
		var parentCaptor = ArgumentCaptor.forClass(TreeNode.class);

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		// expect "scanning..."
		verify(toolWindowMock).getProductNode(ProductConstants.DISPLAYED_CODE_SECURITY);
		verify(toolWindowMock).getProductNode(ProductConstants.DISPLAYED_CODE_QUALITY);

		// if no issues found, we don't need to display the "no fixable issues" node
		verify(toolWindowMock, times(2)).addInfoNode(parentCaptor.capture(), infoNodeCaptor.capture());

		assertEquals(codeSecurityProductNode.getValue(), parentCaptor.getAllValues().get(0).getValue());
		assertEquals(codeQualityProductNode.getValue(), parentCaptor.getAllValues().get(1).getValue());

		assertEquals(ISnykToolView.CONGRATS_NO_ISSUES_FOUND, infoNodeCaptor.getAllValues().get(0).getValue());
		assertEquals(ISnykToolView.CONGRATS_NO_ISSUES_FOUND, infoNodeCaptor.getAllValues().get(1).getValue());
	}

	@Test
	void testSnykScanSuccessAddsInfoNodes_IssuesFound_nothingFixable() {
		var param = new SnykScanParam();
		param.setStatus("success");
		param.setProduct(ProductConstants.SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");
		TreeNode codeSecurityProductNode = new TreeNode(ProductConstants.DISPLAYED_CODE_SECURITY);
		TreeNode codeQualityProductNode = new TreeNode(ProductConstants.DISPLAYED_CODE_QUALITY);

		when(toolWindowMock.getProductNode(ProductConstants.DISPLAYED_CODE_SECURITY))
				.thenReturn(codeSecurityProductNode);
		when(toolWindowMock.getProductNode(ProductConstants.DISPLAYED_CODE_QUALITY)).thenReturn(codeQualityProductNode);
		var infoNodeCaptor = ArgumentCaptor.forClass(TreeNode.class);
		var parentCaptor = ArgumentCaptor.forClass(TreeNode.class);

		var dummyFilePath = Paths.get(param.getFolderPath(), "d").toString();
		var additionalData = Instancio.of(AdditionalData.class).set(Select.field(AdditionalData::isSecurityType), true)
				.set(Select.field(AdditionalData::isUpgradable), false)
				.set(Select.field(AdditionalData::hasAIFix), false).create();

		var cache = new SnykIssueCache();
		Set<Issue> issues = Instancio.of(Issue.class).set(Select.field(Issue::additionalData), additionalData).stream()
				.limit(3).collect(Collectors.toSet());
		cache.addCodeIssues(dummyFilePath, issues);
		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.setIssueCache(cache);

		cut.snykScan(param);

		// expect "scanning..."
		verify(toolWindowMock).getProductNode(ProductConstants.DISPLAYED_CODE_SECURITY);
		verify(toolWindowMock).getProductNode(ProductConstants.DISPLAYED_CODE_QUALITY);

		// if no issues found, we don't need to display the "no fixable issues" node
		verify(toolWindowMock, times(3)).addInfoNode(parentCaptor.capture(), infoNodeCaptor.capture());

		assertTrue(parentCaptor.getAllValues().contains(codeQualityProductNode), "Quality Node was not updated");
		assertTrue(parentCaptor.getAllValues().contains(codeSecurityProductNode), "Security Node was not updated");

		assertEquals("✋ 3 issues found by Snyk", infoNodeCaptor.getAllValues().get(0).getValue());
		assertEquals(ISnykToolView.NO_FIXABLE_ISSUES, infoNodeCaptor.getAllValues().get(1).getValue());
		assertEquals(ISnykToolView.CONGRATS_NO_ISSUES_FOUND, infoNodeCaptor.getAllValues().get(2).getValue());
	}

	@Test
	void testSnykScanAddsToScanStateHashMap() {
		var scanState = ScanState.getInstance();
		var param = new SnykScanParam();
		param.setStatus("inProgress");
		param.setProduct(ProductConstants.SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		var expectedKey = new ScanInProgressKey("a/b/c", ProductConstants.SCAN_PARAMS_CODE);
		var actualState = scanState.isScanInProgress(expectedKey);
		assertEquals(true, actualState);

		param = new SnykScanParam();
		param.setStatus("success");
		param.setProduct(ProductConstants.SCAN_PARAMS_CODE);
		param.setFolderPath("a/b/c");

		cut = new SnykExtendedLanguageClient();
		cut.setToolWindow(toolWindowMock);
		cut.snykScan(param);

		expectedKey = new ScanInProgressKey("a/b/c", ProductConstants.SCAN_PARAMS_CODE);
		actualState = scanState.isScanInProgress(expectedKey);
		assertEquals(false, actualState);
	}
}
