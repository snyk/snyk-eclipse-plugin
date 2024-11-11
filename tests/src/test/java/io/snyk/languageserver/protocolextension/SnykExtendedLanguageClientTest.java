package io.snyk.languageserver.protocolextension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.net.URI;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.analytics.AnalyticsEvent;
import io.snyk.eclipse.plugin.analytics.AnalyticsSender;
import io.snyk.eclipse.plugin.properties.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.LsBaseTest;
import io.snyk.languageserver.ScanInProgressKey;
import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.SnykIssueCache;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

class SnykExtendedLanguageClientTest extends LsBaseTest {
	private InMemoryPreferenceStore store = new InMemoryPreferenceStore();
	private SnykExtendedLanguageClient cut;
	private Preferences pref;
	@BeforeEach
	protected void setUp() {
		store = new InMemoryPreferenceStore();
		pref = Preferences.getInstance(store);
		// we don't want the wizard to pop up, so we set a dummy token
		pref.store(Preferences.AUTH_TOKEN_KEY, "dummy");
	}

	@Test
	void testAddTrustedPathsAddsPathToPreferenceStore() {
		SnykTrustedFoldersParams param = new SnykTrustedFoldersParams();
		param.setTrustedFolders(new String[] { "trusted/path " });

		cut = new SnykExtendedLanguageClient();
		cut.addTrustedPaths(param);

		assertEquals("trusted/path", store.getString(Preferences.TRUSTED_FOLDERS, ""));
	}

	@Test
	void testAddTrustedPathsDeduplicatesAndTrims() {
		SnykTrustedFoldersParams param = new SnykTrustedFoldersParams();
		param.setTrustedFolders(new String[] { "trusted/path", "trusted/path", " trusted/path " });

		cut = new SnykExtendedLanguageClient();
		cut.addTrustedPaths(param);

		assertEquals("trusted/path", store.getString(Preferences.TRUSTED_FOLDERS, ""));
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
		var issueCache = SnykIssueCache.getInstance();
		var uri = "file:///a/b/c/";
		var filePath = LSPEclipseUtils.fromUri(URI.create(uri)).getAbsolutePath();

		var param = new PublishDiagnosticsParams();
		param.setUri(uri);
		var diagnostic = new Diagnostic();
		diagnostic.setSource("Snyk Code");

        var issue = new Issue();
        issue.setTitle("myIssue");
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
		var future = cut.publishDiagnostics316(param);
		try {
			future.get(5, TimeUnit.SECONDS);
		}
		catch (Exception ex){

		}
		var actualIssueList = issueCache.getCodeIssuesForPath(filePath);
		assertEquals(1, actualIssueList.size());
		assertEquals("code", actualIssueList.stream().findFirst().get().getProduct());
		
		// Test remove from cache
		var issueList = List.of(new Issue());
		issueCache.addCodeIssues(filePath, issueList);

		param = new PublishDiagnosticsParams();
		param.setUri(uri);

		cut = new SnykExtendedLanguageClient();
		future = cut.publishDiagnostics316(param);
		try {
			future.get(5, TimeUnit.SECONDS);
		}
		catch (Exception ex){

		}
		assertEquals(true, issueCache.getCodeIssuesForPath(filePath).isEmpty());
	}
	
	@Test
	void testSnykScanAddsToScanStateHashMap() {
		var scanState = ScanState.getInstance();
		var param = new SnykScanParam();
		param.setStatus("inProgress");
		param.setProduct("code");
		param.setFolderPath("a/b/c");
		
		cut = new SnykExtendedLanguageClient();
		cut.snykScan(param);
		
		var expectedKey = new ScanInProgressKey("a/b/c", "code");
		var actualState = scanState.getScanInProgress().get(expectedKey);
		assertEquals(true, actualState);
		
		param = new SnykScanParam();
		param.setStatus("success");
		param.setProduct("code");
		param.setFolderPath("a/b/c");
		
		cut = new SnykExtendedLanguageClient();
		cut.snykScan(param);
		
		expectedKey = new ScanInProgressKey("a/b/c", "code");
		actualState = scanState.getScanInProgress().get(expectedKey);
		assertEquals(false, actualState);
	}
}
