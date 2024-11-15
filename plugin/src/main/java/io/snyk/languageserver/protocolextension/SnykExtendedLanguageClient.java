package io.snyk.languageserver.protocolextension;

import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_QUALITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DISPLAYED_CODE_SECURITY;
import static io.snyk.eclipse.plugin.domain.ProductConstants.LSP_SOURCE_TO_SCAN_PARAMS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_CODE;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_IAC;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_OSS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_PARAMS_TO_DISPLAYED;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_ERROR;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_IN_PROGRESS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_SUCCESS;
import static io.snyk.eclipse.plugin.properties.preferences.Preferences.FILTER_IGNORES_IGNORED_ISSUES;
import static io.snyk.eclipse.plugin.properties.preferences.Preferences.FILTER_IGNORES_OPEN_ISSUES;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.CONGRATS_NO_ISSUES_FOUND;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.NODE_TEXT_SCANNING;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.NO_FIXABLE_ISSUES;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.getPlural;

import java.io.File;
import java.net.URI;
import java.nio.file.InvalidPathException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.analytics.AbstractAnalyticsEvent;
import io.snyk.eclipse.plugin.analytics.AnalyticsEvent;
import io.snyk.eclipse.plugin.analytics.AnalyticsSender;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.SnykView;
import io.snyk.eclipse.plugin.views.snyktoolview.BaseTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.LsCommandID;
import io.snyk.languageserver.LsNotificationID;
import io.snyk.languageserver.ScanInProgressKey;
import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.SnykIssueCache;
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

@SuppressWarnings("restriction")
public class SnykExtendedLanguageClient extends LanguageClientImpl {
	private final ProgressManager progressMgr = new ProgressManager();
	private final ObjectMapper om = new ObjectMapper();
	private AnalyticsSender analyticsSender;
	private ISnykToolView toolView;
	private SnykIssueCache issueCache = SnykIssueCache.getInstance();

	private static SnykExtendedLanguageClient instance = null;

	public SnykExtendedLanguageClient() {
		super();
		instance = this;
		sendPluginInstalledEvent();
		om.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
	}

	private void sendPluginInstalledEvent() {
		if (!Preferences.getInstance().getBooleanPref(Preferences.ANALYTICS_PLUGIN_INSTALLED_SENT, false)) {
			if (analyticsSender == null) {
				analyticsSender = AnalyticsSender.getInstance();
			}
			var pluginInstalledEvent = new AnalyticsEvent("plugin installed", List.of("install"));
			analyticsSender.logEvent(pluginInstalledEvent, new Consumer<Void>() {
				@Override
				public void accept(Void t) {
					Preferences.getInstance().store(Preferences.ANALYTICS_PLUGIN_INSTALLED_SENT, "true");
				}
			});
		}
	}

	public static SnykExtendedLanguageClient getInstance() {
		return instance; // we leave instantiation to LSP4e, no lazy construction here
	}

	public LanguageServer getConnectedLanguageServer() {
		return super.getLanguageServer();
	}

	public void triggerScan(IWorkbenchWindow window) {
		CompletableFuture.runAsync(() -> {
			if (Preferences.getInstance().getAuthToken().isBlank()) {
				runSnykWizard();
			} else {
				try {
					if (window == null) {
						executeCommand(LsCommandID.SNYK_WORKSPACE_SCAN, new ArrayList<>());
						return;
					}

					ISelectionService service = window.getSelectionService();
					IStructuredSelection structured = (IStructuredSelection) service.getSelection();

					Object firstElement = structured.getFirstElement();
					IProject project = null;
					if (firstElement instanceof JavaProject) {
						project = ((JavaProject) firstElement).getProject();
					}

					if (firstElement instanceof IProject) {
						project = (IProject) firstElement;
					}

					if (project != null) {
						runForProject(project.getName());
						executeCommand(LsCommandID.SNYK_WORKSPACE_FOLDER_SCAN,
								List.of(project.getLocation().toOSString()));
					}
				} catch (Exception e) {
					SnykLogger.logError(e);
				}
			}
		});
	}

	public void triggerAuthentication() {
		executeCommand(LsCommandID.SNYK_LOGIN, new ArrayList<>());
	}

	public void ensureLanguageServerRunning() {
		boolean wait = true;
		while (wait && !Thread.interrupted()) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				SnykLogger.logInfo("waiting for language server startup was interrupted");
				break;
			}

			try {
				SnykLanguageServer.startSnykLanguageServer();
			} catch (Exception e) {
				SnykLogger.logError(e);
			}
			wait = getConnectedLanguageServer() == null;
		}
	}

	public void trustWorkspaceFolders() {
		executeCommand(LsCommandID.SNYK_TRUST_WORKSPACE_FOLDERS, new ArrayList<>());
	}

	public boolean getSastEnabled() {
		try {
			CompletableFuture<Object> lsSastSettings = executeCommand(LsCommandID.SNYK_SAST_ENABLED, new ArrayList<>());
			Object result;
			try {
				result = lsSastSettings.get(5, TimeUnit.SECONDS);
			} catch (TimeoutException e) {
				SnykLogger.logInfo("did not get a response for sast settings, disabling Snyk Code");
				return false;
			}
			SastSettings sastSettings = om.convertValue(result, SastSettings.class);
			return sastSettings != null ? sastSettings.sastEnabled : false;
		} catch (Exception e) {
			SnykLogger.logError(e);
		}

		return false;
	}

	public String getIssueDescription(String issueId) {
		if (StringUtils.isEmpty(issueId)) {
			SnykLogger.logInfo("issueId is empty");
			return "";
		}
		CompletableFuture<Object> issueDescription = executeCommand(LsCommandID.SNYK_GENERATE_ISSUE_DESCRIPTION,
				List.of(issueId));
		Object result;
		try {
			result = issueDescription.get(5, TimeUnit.SECONDS);
		} catch (Exception ex) {
			SnykLogger.logInfo(
					"did not get issue description for issue " + issueId + "\n" + ExceptionUtils.getStackTrace(ex));
			return "";
		}

		return String.valueOf(result);
	}

	@JsonNotification(value = LsNotificationID.SNYK_HAS_AUTHENTICATED)
	public void hasAuthenticated(HasAuthenticatedParam param) {
		var prefs = Preferences.getInstance();

		var oldToken = prefs.getAuthToken();
		var oldApi = prefs.getEndpoint();

		if (param.getApiUrl() != null && !param.getApiUrl().isBlank() && !param.getApiUrl().equals(oldApi)) {
			prefs.store(Preferences.ENDPOINT_KEY, param.getApiUrl());
		}

		String newToken = param.getToken();
		boolean differentToken = newToken != oldToken;

		if (differentToken) {
			prefs.store(Preferences.AUTH_TOKEN_KEY, newToken);
		}

		if (!newToken.isBlank() && PlatformUI.isWorkbenchRunning()) {
			enableSnykViewRunActions();
		}

		if (differentToken && !newToken.isBlank()) {
			triggerScan(null);
		}
	}

	@JsonNotification(value = LsNotificationID.SNYK_IS_AVAILABLE_CLI)
	public void isAvailableCli(SnykIsAvailableCliParams param) {
		Preferences.getInstance().store(Preferences.CLI_PATH, param.getCliPath());
		enableSnykViewRunActions();
	}

	@JsonNotification(value = LsNotificationID.SNYK_ADD_TRUSTED_FOLDERS)
	public void addTrustedPaths(SnykTrustedFoldersParams param) {
		var prefs = Preferences.getInstance();
		var storedTrustedPaths = prefs.getPref(Preferences.TRUSTED_FOLDERS, "");
		var trustedPaths = storedTrustedPaths.split(File.pathSeparator);
		var pathSet = new HashSet<>(Arrays.asList(trustedPaths));
		pathSet.addAll(Arrays.asList(param.getTrustedFolders()));
		Preferences.getInstance().store(Preferences.TRUSTED_FOLDERS, pathSet.stream().filter(s -> !s.isBlank())
				.map(s -> s.trim()).distinct().collect(Collectors.joining(File.pathSeparator)));
	}

	@JsonNotification(value = LsNotificationID.SNYK_SCAN)
	public void snykScan(SnykScanParam param) {
		var inProgressKey = new ScanInProgressKey(param.getFolderPath(), param.getProduct());
		var scanState = ScanState.getInstance();

		Display.getDefault().syncExec(() -> {
			if (toolView == null && !Preferences.getInstance().isTest()) {
				IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
				toolView = (ISnykToolView) activePage.findView(SnykToolView.ID);
			}
		});
		switch (param.getStatus()) {
		case SCAN_STATE_IN_PROGRESS:
			scanState.setScanInProgress(inProgressKey, true);
			setScanningState(param);
			break;
		case SCAN_STATE_SUCCESS:
			scanState.setScanInProgress(inProgressKey, false);
			var displayProduct = SCAN_PARAMS_TO_DISPLAYED.get(param.getProduct());
			if (displayProduct != null) {
				addInfoNodes(displayProduct);
			} else {
				// if we don't have a direct mapping, it can only be code, as the code scan
				// bundles
				// quality issues and security issues. We then have to update both root nodes.
				addInfoNodes(DISPLAYED_CODE_SECURITY);
				addInfoNodes(DISPLAYED_CODE_QUALITY);
			}
			// Show scanning finished state
			break;
		case SCAN_STATE_ERROR:
			scanState.setScanInProgress(inProgressKey, false);
			// Show error state
			break;
		}
	}

	private void setScanningState(SnykScanParam param) {
		String displayProduct = SCAN_PARAMS_TO_DISPLAYED.get(param.getProduct());
		var productNode = toolView.getProductNode(displayProduct);
		if (displayProduct != null) {
			toolView.resetNode(productNode);
			toolView.setNodeText(productNode, NODE_TEXT_SCANNING);
		} else {
			toolView.resetNode(productNode);
			var secNode = toolView.getProductNode(DISPLAYED_CODE_SECURITY);
			var qualNode = toolView.getProductNode(DISPLAYED_CODE_QUALITY);
			
			toolView.resetNode(secNode);
			toolView.resetNode(qualNode);
			toolView.setNodeText(secNode, NODE_TEXT_SCANNING);
			toolView.setNodeText(qualNode, NODE_TEXT_SCANNING);
		}
	}

	private void addInfoNodes(String displayProduct) {
		BaseTreeNode productNode = toolView.getProductNode(displayProduct);
		toolView.resetNode(productNode);

		long totalCount = issueCache.getTotalCount(displayProduct);
		long fixableCount = issueCache.getFixableCount(displayProduct);
		long ignoredCount = issueCache.getIgnoredCount(displayProduct);
		if (totalCount == 0) {
			toolView.addInfoNode(productNode, new BaseTreeNode(CONGRATS_NO_ISSUES_FOUND));
		} else {
			String text = "✋ " + totalCount + " issue" + getPlural(totalCount) + " found by Snyk";
			if (ignoredCount > 0) {
				text += ", " + ignoredCount + " ignored";
			}
			toolView.addInfoNode(productNode, new BaseTreeNode(text));
		}
		if (totalCount > 0 && fixableCount == 0) {
			toolView.addInfoNode(productNode, new BaseTreeNode(NO_FIXABLE_ISSUES));
		}

		if (totalCount > 0 && fixableCount > 0) {
			toolView.addInfoNode(productNode, new BaseTreeNode(
					"⚡️ " + fixableCount + " issue" + getPlural(fixableCount) + " can be fixed automatically"));
		}

		if (totalCount > 0 && ignoredCount == totalCount
				&& Preferences.getInstance().getBooleanPref(FILTER_IGNORES_OPEN_ISSUES)) {
			toolView.addInfoNode(productNode,
					new BaseTreeNode("Adjust your Issue View Options to see ignored issues."));
		}

		if (totalCount > 0 && ignoredCount == 0
				&& Preferences.getInstance().getBooleanPref(FILTER_IGNORES_IGNORED_ISSUES)) {
			toolView.addInfoNode(productNode, new BaseTreeNode("Adjust your Issue View Options to see open issues."));
		}
	}

	@JsonNotification(value = LsNotificationID.SNYK_PUBLISH_DIAGNOSTICS_316)
	public CompletableFuture<Void> publishDiagnostics316(PublishDiagnosticsParams param) {
		return CompletableFuture.runAsync(() -> {
			var uri = param.getUri();
			if (StringUtils.isEmpty(uri)) {
				SnykLogger.logInfo("uri for PublishDiagnosticsParams is empty");
				return;
			}
			var filePath = LSPEclipseUtils.fromUri(URI.create(uri)).getAbsolutePath();
			if (filePath == null) {
				SnykLogger.logError(new InvalidPathException(uri, "couldn't resolve uri " + uri + " to file"));
				return;
			}
			List<Diagnostic> diagnostics = param.getDiagnostics();
			if (diagnostics.isEmpty()) {
				issueCache.removeAllIssuesForPath(filePath);
				return;
			}
			var source = diagnostics.get(0).getSource();
			if (StringUtils.isEmpty(source)) {
				return;
			}
			var snykProduct = LSP_SOURCE_TO_SCAN_PARAMS.get(source);
			List<Issue> issueList = new ArrayList<>();

			for (var diagnostic : diagnostics) {
				Issue issue;
				if (diagnostic.getData() == null) {
					continue;
				}
				try {
					issue = om.readValue(diagnostic.getData().toString(), Issue.class);
					issueList.add(issue);
				} catch (JsonProcessingException e) {
					SnykLogger.logError(e);
					continue;
				}
			}

			switch (snykProduct) {
			case SCAN_PARAMS_CODE:
				issueCache.addCodeIssues(filePath, issueList);
				break;
			case SCAN_PARAMS_OSS:
				issueCache.addOssIssues(filePath, issueList);
				break;
			case SCAN_PARAMS_IAC:
				issueCache.addIacIssues(filePath, issueList);
				break;
			}
		});
	}

	public void reportAnalytics(AbstractAnalyticsEvent event) {
		try {
			var eventString = om.writeValueAsString(event);
			executeCommand(LsCommandID.SNYK_REPORT_ANALYTICS, List.of(eventString));
		} catch (Exception e) {
			SnykLogger.logError(e);
		}

	}

	@Override
	public CompletableFuture<Void> createProgress(WorkDoneProgressCreateParams params) {
		return progressMgr.createProgress(params);
	}

	@Override
	public void notifyProgress(ProgressParams params) {
		progressMgr.updateProgress(params);
	}

	private void runSnykWizard() {
		SnykWizard wizard = new SnykWizard();

		WizardDialog dialog = new WizardDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard);

		dialog.setBlockOnOpen(true);
		dialog.open();
	}

	private void enableSnykViewRunActions() {
		PlatformUI.getWorkbench().getDisplay().asyncExec(() -> {
			var snykView = SnykStartup.getSnykView();
			if (snykView != null)
				snykView.toggleRunActionEnablement();
		});
	}

	private void runForProject(String projectName) {
		SnykView snykView = SnykStartup.getSnykView();
		if (snykView != null) {
			snykView.testProject(projectName);
		}
	}

	private CompletableFuture<Object> executeCommand(@NonNull String command, List<Object> arguments) {
		ensureLanguageServerRunning();
		ExecuteCommandParams params = new ExecuteCommandParams(command, arguments);
		try {
			return getLanguageServer().getWorkspaceService().executeCommand(params);
		} catch (Exception e) {
			SnykLogger.logError(e);
		}
		return CompletableFuture.completedFuture(null);
	}

	/**
	 * Refresh the token using language server. Waits up to 2s for the token change.
	 *
	 * @return true if token has changed, false if not
	 */
	public boolean refreshOAuthToken() {
		var p = Preferences.getInstance();
		if (p.isTest()) {
			return true;
		}
		var token = p.getAuthToken();
		final int timeout = 5;
		CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
			var result = executeCommand(LsCommandID.SNYK_GET_ACTIVE_USER, new ArrayList<>());
			// we don't wait forever, and if we can't get a user name (refresh token), we're
			// done.
			try {
				result.get(timeout - 1, TimeUnit.SECONDS);
			} catch (InterruptedException | ExecutionException | TimeoutException e) {
				return "";
			}
			while (token.equals(p.getAuthToken())) {
				try {
					Thread.sleep(10);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
			return p.getAuthToken();
		});

		// wait until token has changed or 2s have passed
		var newToken = future.completeOnTimeout(token, 5, TimeUnit.SECONDS).join();
		return !token.equals(newToken);
	}

	static class SastSettings {
		public boolean sastEnabled;

		public LocalCodeEngine localCodeEngine;

		public boolean reportFalsePositivesEnabled;

		public String org;

		public boolean autofixEnabled;
	}

	/**
	 * SAST local code engine configuration.
	 */
	static class LocalCodeEngine {
		public boolean enabled;

		public String url;
	}

	public static <T> T convertInstanceOfObject(Object o, Class<T> clazz) {
		try {
			return clazz.cast(o);
		} catch (ClassCastException e) {
			return null;
		}
	}

	public void setToolWindow(ISnykToolView toolView) {
		this.toolView = toolView;
	}

	public void setIssueCache(SnykIssueCache cache) {
		this.issueCache = cache;
	}
}
