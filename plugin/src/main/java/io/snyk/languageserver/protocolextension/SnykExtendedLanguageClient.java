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
import static io.snyk.eclipse.plugin.domain.ProductConstants.SEVERITY_CRITICAL;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SEVERITY_HIGH;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SEVERITY_LOW;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SEVERITY_MEDIUM;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.NODE_TEXT_SCANNING;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.ShowDocumentParams;
import org.eclipse.lsp4j.ShowDocumentResult;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.WorkDoneProgressKind;
import org.eclipse.lsp4j.WorkDoneProgressNotification;
import org.eclipse.lsp4j.WorkspaceFolder;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.analytics.AbstractTask;
import io.snyk.eclipse.plugin.analytics.AnalyticsEventTask;
import io.snyk.eclipse.plugin.analytics.TaskProcessor;
import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigs;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.snyktoolview.FileTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.InfoTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.IssueTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.ProductTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.CommandHandler;
import io.snyk.languageserver.FeatureFlagConstants;
import io.snyk.languageserver.IssueCacheHolder;
import io.snyk.languageserver.LsConfigurationUpdater;
import io.snyk.languageserver.LsConstants;
import io.snyk.languageserver.ScanInProgressKey;
import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.SnykIssueCache;
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.protocolextension.messageObjects.Diagnostic316;
import io.snyk.languageserver.protocolextension.messageObjects.FeatureFlagStatus;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfigsParam;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.LsSdk;
import io.snyk.languageserver.protocolextension.messageObjects.PublishDiagnostics316Param;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;
import io.snyk.languageserver.protocolextension.messageObjects.SummaryPanelParams;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

@SuppressWarnings("restriction")
public class SnykExtendedLanguageClient extends LanguageClientImpl {
	private static final String MARKER_TYPE = "io.snyk.languageserver.marker";
	private ProgressManager progressManager = new ProgressManager(this);
	private final ObjectMapper om = new ObjectMapper();
	private TaskProcessor taskProcessor;
	private ISnykToolView toolView;
	// this field is for testing only
	private LanguageServer ls;
	private LsConfigurationUpdater configurationUpdater = new LsConfigurationUpdater();
	private Object chSyncObject = new Object();
	private CommandHandler commandHandler;

	private static SnykExtendedLanguageClient instance;

	public SnykExtendedLanguageClient() {
		super();
		om.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		instance = this; // NOPMD
		CompletableFuture.runAsync(() -> {
			SnykLanguageServer.waitForInit();
			registerPluginInstalledEventTask();
			registerRefreshFeatureFlagsTask();
		});
	}

	public static SnykExtendedLanguageClient getInstance() {
		return instance; // we leave instantiation to LSP4e, no lazy
							// construction here
	}

	public LanguageServer getConnectedLanguageServer() {
		if (this.ls != null) {
			return ls;
		}
		return super.getLanguageServer();
	}

	public void updateConfiguration() {
		this.configurationUpdater.configurationChanged();
		if (this.toolView != null) {
			this.toolView.refreshBrowser(null);
		}
	}

	@Override
	public CompletableFuture<List<WorkspaceFolder>> workspaceFolders() {
		return CompletableFuture.completedFuture(ResourceUtils.getAccessibleTopLevelProjects().stream()
				.map(LSPEclipseUtils::toWorkspaceFolder).toList());
	}

	private void registerRefreshFeatureFlagsTask() {
		if (taskProcessor == null) {
			taskProcessor = TaskProcessor.getInstance();
		}
		Consumer<SnykExtendedLanguageClient> refreshFeatureFlagsConsumer = lc -> lc.refreshFeatureFlags();
		taskProcessor.registerTask(refreshFeatureFlagsConsumer, null);
	}

	public void refreshFeatureFlags() {
		boolean enableConsistentIgnores = getFeatureFlagStatus(FeatureFlagConstants.SNYK_CODE_CONSISTENT_IGNORES);
		toggleIgnores(enableConsistentIgnores);
	}

	private void toggleIgnores(Boolean enableConsistentIgnores) {
		if (Preferences.getInstance().isTest()) {
			return;
		}
		Preferences.getInstance().store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED,
				Boolean.valueOf(enableConsistentIgnores).toString());
		PlatformUI.getWorkbench().getDisplay().asyncExec(() -> {
			var snykToolView = SnykStartup.getView();
			if (snykToolView != null)
				snykToolView.toggleIgnoresButtons();
		});
	}

	private void registerPluginInstalledEventTask() {
		if (!Preferences.getInstance().getBooleanPref(Preferences.ANALYTICS_PLUGIN_INSTALLED_SENT, false)) {
			if (taskProcessor == null) {
				taskProcessor = TaskProcessor.getInstance();
			}
			var pluginInstalledEvent = new AnalyticsEventTask("plugin installed", List.of("install"));
			Consumer<SnykExtendedLanguageClient> analyticsTask = lc -> lc.reportAnalytics(pluginInstalledEvent);
			Consumer<Void> analyticsCallback = v -> {
				Preferences.getInstance().store(Preferences.ANALYTICS_PLUGIN_INSTALLED_SENT, "true");
			};

			taskProcessor.registerTask(analyticsTask, analyticsCallback);
		}
	}

	public void triggerScan(Path projectPath) {
		CompletableFuture.runAsync(() -> {
			if (Preferences.getInstance().getAuthToken().isBlank()) {
				SnykWizard.createAndLaunch();
			} else {
				updateConfiguration();
				openToolView();
				try {
					if (projectPath != null) {
						executeCommand(LsConstants.COMMAND_WORKSPACE_FOLDER_SCAN, List.of(projectPath));
						return;
					}

					executeCommand(LsConstants.COMMAND_WORKSPACE_SCAN, new ArrayList<>());
				} catch (Exception e) {
					SnykLogger.logError(e);
				}
			}
		});
	}

	private CompletableFuture<Object> executeCommand(String cmd, List<Object> args) {
		synchronized (chSyncObject) {
			if (commandHandler == null) {
				commandHandler = CommandHandler.getInstance();
			}
		}
		return commandHandler.executeCommand(cmd, args);
	}

	public void triggerAuthentication() {
		executeCommand(LsConstants.COMMAND_LOGIN, new ArrayList<>());
	}

	public void ensureLanguageServerRunning() {
		SnykLanguageServer.waitForInit();
		// now trigger start up if it's not already triggered
		while (getConnectedLanguageServer() == null && !Thread.interrupted()) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				SnykLogger.logInfo("waiting for language server startup was interrupted");
				Thread.currentThread().interrupt();
				break;
			}

			try {
				SnykLanguageServer.startSnykLanguageServer();
			} catch (Exception e) {
				SnykLogger.logError(e);
			}
		}
	}

	public void trustWorkspaceFolders() {
		executeCommand(LsConstants.COMMAND_TRUST_WORKSPACE_FOLDERS, new ArrayList<>());
	}

	public boolean getSastEnabled() {
		try {
			CompletableFuture<Object> lsSastSettings = executeCommand(LsConstants.COMMAND_GET_SETTINGS_SAST_ENABLED,
					new ArrayList<>());
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

	public boolean getFeatureFlagStatus(String featureFlag) {
		try {
			CompletableFuture<Object> lsGlobalIgnoresFeatureFlag = executeCommand(
					LsConstants.COMMAND_GET_FEATURE_FLAG_STATUS, List.of(featureFlag));
			Object result;
			try {
				result = lsGlobalIgnoresFeatureFlag.get(5, TimeUnit.SECONDS);
			} catch (TimeoutException e) {
				return false;
			}
			FeatureFlagStatus featureFlagStatus = om.convertValue(result, FeatureFlagStatus.class);
			return featureFlagStatus != null ? featureFlagStatus.getOk() : false;
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
		CompletableFuture<Object> issueDescription = executeCommand(LsConstants.COMMAND_GENERATE_ISSUE_DESCRIPTION,
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

	public void sendCodeFixDiffsCommand(String folderURI, String fileURI, String issueID) {
		executeCommand(LsConstants.COMMAND_CODE_FIX_DIFFS, List.of(folderURI, fileURI, issueID));
	}

	public void sendCodeApplyAiFixEditCommand(String fixId) {
		executeCommand(LsConstants.COMMAND_CODE_FIX_APPLY_AI_EDIT, List.of(fixId));
	}

	@JsonNotification(value = LsConstants.SNYK_HAS_AUTHENTICATED)
	public void hasAuthenticated(HasAuthenticatedParam param) {
		var prefs = Preferences.getInstance();

		var oldToken = prefs.getAuthToken();
		var oldApi = prefs.getEndpoint();

		if (param.getApiUrl() != null && !param.getApiUrl().isBlank() && !param.getApiUrl().equals(oldApi)) {
			prefs.store(Preferences.ENDPOINT_KEY, param.getApiUrl());
		}

		String newToken = param.getToken();
		boolean differentToken = !newToken.equals(oldToken);

		if (differentToken) {
			prefs.store(Preferences.AUTH_TOKEN_KEY, newToken);
		}

		if (!Preferences.getInstance().isTest()) {
			configurationUpdater.configurationChanged();
			refreshFeatureFlags();
		}

		if (differentToken && !newToken.isBlank()) {
			triggerScan(null);
		}
	}

	@JsonNotification(value = LsConstants.SNYK_IS_AVAILABLE_CLI)
	public void isAvailableCli(SnykIsAvailableCliParams param) {
		Preferences.getInstance().store(Preferences.CLI_PATH, param.getCliPath());
	}

	@JsonNotification(value = LsConstants.SNYK_ADD_TRUSTED_FOLDERS)
	public void addTrustedPaths(SnykTrustedFoldersParams param) {
		var prefs = Preferences.getInstance();
		var storedTrustedPaths = prefs.getPref(Preferences.TRUSTED_FOLDERS, "");
		var trustedPaths = storedTrustedPaths.split(File.pathSeparator);
		var pathSet = new HashSet<>(Arrays.asList(trustedPaths));
		pathSet.addAll(Arrays.asList(param.getTrustedFolders()));
		Preferences.getInstance().store(Preferences.TRUSTED_FOLDERS, pathSet.stream().filter(s -> !s.isBlank())
				.map(s -> s.trim()).distinct().collect(Collectors.joining(File.pathSeparator)));
	}

	@JsonNotification(value = LsConstants.SNYK_SCAN)
	public void snykScan(SnykScanParam param) {
		var inProgressKey = new ScanInProgressKey(param.getFolderPath(), param.getProduct());
		var scanState = ScanState.getInstance();
		SnykIssueCache issueCache = null;
		if (!param.getFolderPath().isBlank()) {
			issueCache = IssueCacheHolder.getInstance().getCacheInstance(param.getFolderPath());
		}
		openToolView();

		Set<ProductTreeNode> affectedProductTreeNodes = getAffectedProductNodes(param.getProduct(),
				param.getFolderPath());

		if (affectedProductTreeNodes.isEmpty()) {
			if (toolView != null && toolView.getRoot() != null) {
				toolView.getRoot().reset();
				affectedProductTreeNodes = getAffectedProductNodes(param.getProduct(), param.getFolderPath());
			}
		}

		switch (param.getStatus()) {
		case SCAN_STATE_IN_PROGRESS:
			scanState.setScanInProgress(inProgressKey, true);
			break;
		case SCAN_STATE_SUCCESS:
			scanState.setScanInProgress(inProgressKey, false);
			for (ProductTreeNode productTreeNode : affectedProductTreeNodes) {
				this.toolView.resetNode(productTreeNode);
				addInfoNodes(productTreeNode, param.getFolderPath(), issueCache);
				populateFileAndIssueNodes(productTreeNode, issueCache);
			}
			break;
		case SCAN_STATE_ERROR:
			scanState.setScanInProgress(inProgressKey, false);
			for (ProductTreeNode productTreeNode : affectedProductTreeNodes) {
				productTreeNode.setErrorMessage(param.getErrorMessage());
			}
			break;
		default:
			break;
		}
		setNodeState(param.getStatus(), affectedProductTreeNodes, issueCache);
		this.toolView.refreshBrowser(param.getStatus());
	}

	@JsonNotification(value = LsConstants.SNYK_FOLDER_CONFIG)
	public void folderConfig(FolderConfigsParam folderConfigParam) {
		List<FolderConfig> folderConfigs = folderConfigParam != null ? folderConfigParam.getFolderConfigs() : List.of();
		CompletableFuture.runAsync(() -> FolderConfigs.getInstance().addAll(folderConfigs));
	}

	@JsonNotification(value = LsConstants.SNYK_SCAN_SUMMARY)
	public void updateSummaryPanel(SummaryPanelParams summary) {
		CompletableFuture.runAsync(() -> {
			openToolView();
			this.toolView.updateSummary(summary.getSummary());
		});
	}

	@Override
	public CompletableFuture<ShowDocumentResult> showDocument(ShowDocumentParams params) {
		URI uri;
		try {
			uri = new URI(params.getUri());
		} catch (URISyntaxException e) {
			SnykLogger.logError(e);
			return null;
		}

		SnykShowFixUriDetails uriDetails = SnykShowFixUriDetails.fromURI(uri);

		Issue issue;
		if (uriDetails.isValid()) {
			issue = getIssueFromCache(uriDetails.product(), uriDetails.issueId());
		} else {
			SnykLogger.logInfo(String.format("Invalid URI: scheme=%s, product=%s, action=%s, issue=%s",
					uriDetails.scheme(), uriDetails.product(), uriDetails.action(), uriDetails.issueId()));
			return super.showDocument(params);
		}

		if (issue == null) {
			SnykLogger.logInfo(String.format("Issue not found in the issueCache; issueId: %s", uriDetails.issueId()));
			return null;
		}

		return CompletableFuture.supplyAsync(() -> {
			openToolView();
			this.toolView.selectTreeNode(issue, issue.filterableIssueType());
			return new ShowDocumentResult(true);
		});
	}

	private Issue getIssueFromCache(String product, String issueId) {
		Issue issue = null;
		IssueCacheHolder issueCacheHolder = IssueCacheHolder.getInstance();
		List<IProject> openProjects = ResourceUtils.getAccessibleTopLevelProjects();
		for (IProject iProject : openProjects) {
			issue = issueCacheHolder.getCacheInstance(iProject)
					.getIssueByDiagnosticProductAndKey(product, issueId);
			if(issue != null) {
				return issue;
			}
		}
		return issue;
	}

	private ISnykToolView openToolView() {
		// we don't want to use the UI in tests usually
		if (this.toolView != null || Preferences.getInstance().isTest()) {
			return null;
		}
		Display.getDefault().syncExec(() -> {
			IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			try {
				toolView = (ISnykToolView) activePage.showView(SnykToolView.ID);
			} catch (PartInitException e) {
				SnykLogger.logError(e);
				return;
			}
		});
		return toolView;
	}

	private Set<ProductTreeNode> getAffectedProductNodes(String snykScanProduct, String folderPath) {
		Preferences pref = Preferences.getInstance();
		Set<ProductTreeNode> affectedProductTreeNodes = new HashSet<>();
		var displayProduct = SCAN_PARAMS_TO_DISPLAYED.get(snykScanProduct);
		if (displayProduct != null) {
			ProductTreeNode productNode = toolView.getProductNode(displayProduct, folderPath);
			if (productNode != null) {
				affectedProductTreeNodes.add(productNode);
			}
		} else {
			ProductTreeNode productNode = toolView.getProductNode(DISPLAYED_CODE_SECURITY, folderPath);
			if (productNode != null && pref.getBooleanPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY)) {
				affectedProductTreeNodes.add(productNode);
			}
			productNode = toolView.getProductNode(DISPLAYED_CODE_QUALITY, folderPath);
			if (productNode != null && pref.getBooleanPref(Preferences.ACTIVATE_SNYK_CODE_QUALITY)) {
				affectedProductTreeNodes.add(productNode);
			}
		}
		return Collections.unmodifiableSet(affectedProductTreeNodes);
	}

	private void setNodeState(String status, Set<ProductTreeNode> affectedProductTreeNodes, SnykIssueCache cache) {
		if (affectedProductTreeNodes.isEmpty()) {
			return;
		}
		String nodeText;

		if (SCAN_STATE_IN_PROGRESS.equals(status)) {
			nodeText = NODE_TEXT_SCANNING;
			setProductNodeText(affectedProductTreeNodes, nodeText);
		} else if (SCAN_STATE_ERROR.equals(status)) {
			nodeText = ISnykToolView.NODE_TEXT_ERROR;
			setProductNodeText(affectedProductTreeNodes, nodeText);
			toolView.refreshTree();
		} else if (SCAN_STATE_SUCCESS.equals(status)) {
			for (ProductTreeNode productTreeNode : affectedProductTreeNodes) {
				var displayCounts = getCountsSuffix(productTreeNode, cache);
				setProductNodeText(Set.of(productTreeNode), displayCounts);
			}
			toolView.refreshTree();
		}
	}

	private void setProductNodeText(Set<ProductTreeNode> nodes, String nodeText) {
		for (ProductTreeNode productTreeNode : nodes) {
			toolView.setNodeText(productTreeNode, nodeText);
		}
	}

	public String getCountsSuffix(ProductTreeNode productTreeNode, SnykIssueCache issueCache) {
		String product = productTreeNode.getProduct();
		var critical = issueCache.getIssueCountBySeverity(product, SEVERITY_CRITICAL);
		var high = issueCache.getIssueCountBySeverity(product, SEVERITY_HIGH);
		var medium = issueCache.getIssueCountBySeverity(product, SEVERITY_MEDIUM);
		var low = issueCache.getIssueCountBySeverity(product, SEVERITY_LOW);
		var total = issueCache.getTotalCount(product);
		return String.format("%d unique vulnerabilities: %d critical, %d high, %d medium, %d low", total, critical,
				high, medium, low);
	}

	private SnykIssueCache getIssueCache(String filePath) {
		var issueCache = IssueCacheHolder.getInstance().getCacheInstance(Paths.get(filePath));
		if (issueCache == null) {
			throw new IllegalArgumentException("No issue cache for param possible");
		}
		return issueCache;
	}

	private void addInfoNodes(ProductTreeNode productNode, String folderPath, SnykIssueCache issueCache) {
		if (productNode == null) {
			SnykLogger.logInfo("given product node is null, not adding info nodes. folderPath: " + folderPath);
			return;
		}
		toolView.removeInfoNodes(productNode);

		long totalIssueCount = issueCache.getTotalCount(productNode.getProduct());
		long ignoredIssueCount = issueCache.getIgnoredCount(productNode.getProduct());
		long openIssueCount = totalIssueCount - ignoredIssueCount;
		var text = productNode.getProduct() == ProductConstants.DISPLAYED_CODE_SECURITY ? getIssueFoundTextForCodeSecurity(totalIssueCount, openIssueCount, ignoredIssueCount) : getIssueFoundText(totalIssueCount);
		toolView.addInfoNode(productNode, new InfoTreeNode(text));

		if (totalIssueCount == 0) {
			var ivoNode = productNode.getProduct() == ProductConstants.DISPLAYED_CODE_SECURITY ? getNoIssueViewOptionsSelectedTreeNodeForCodeSecurity() : getNoIssueViewOptionsSelectedTreeNode();
			if (ivoNode != null) {
				toolView.addInfoNode(productNode, ivoNode);
			}
		} else {
			long fixableIssueCount = issueCache.getFixableCount(productNode.getProduct());
			var fixableNode = productNode.getProduct() == ProductConstants.DISPLAYED_CODE_SECURITY ? getFixableIssuesNodeForCodeSecurity(fixableIssueCount) : getFixableIssuesNode(fixableIssueCount);
			if (fixableNode != null) {
				toolView.addInfoNode(productNode, fixableNode);
			}
		}
	}

	private String getIssueFoundText(long issueCount) {
		var pref = Preferences.getInstance();
		boolean isIgnoresEnabled = pref.getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED);
		boolean showingOpen = pref.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES);
		if (isIgnoresEnabled && !showingOpen) {
			return "Open issues are disabled!";
		}
		if (issueCount == 0) {
			return "✅ Congrats! No issues found!";
		} else {
			return "✋ " + issueCount + " issue" + (issueCount == 1 ? "" : "s");
		}
	}

	private String getIssueFoundTextForCodeSecurity(long totalIssueCount, long openIssueCount, long ignoredIssueCount) {
		var pref = Preferences.getInstance();
		boolean isIgnoresEnabled = pref.getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED);
		if (!isIgnoresEnabled) {
			return getIssueFoundText(totalIssueCount);
		}

		boolean showingOpen = pref.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES);
		boolean showingIgnored = pref.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES);
		String openIssuesText = openIssueCount + " open issue" + (openIssueCount == 1 ? "" : "s");
		String ignoredIssuesText = ignoredIssueCount + " ignored issue" + (ignoredIssueCount == 1 ? "" : "s");
		if (showingOpen) {
			if (showingIgnored) {
				if (totalIssueCount == 0) {
					return "✅ Congrats! No issues found!";
				} else {
					return "✋ " + openIssuesText + ", " + ignoredIssuesText;
				}
			} else {
				if (openIssueCount == 0) {
					return "✅ Congrats! No open issues found!";
				} else {
					return "✋ " + openIssuesText;
				}
			}
		} else if (showingIgnored) {
			if (ignoredIssueCount == 0) {
				return "✋ No ignored issues, open issues are disabled";
			} else {
				return "✋ " + ignoredIssuesText + ", open issues are disabled";
			}
		} else {
			return "Open and Ignored issues are disabled!";
		}
	}

	private InfoTreeNode getNoIssueViewOptionsSelectedTreeNode() {
		var pref = Preferences.getInstance();
		boolean isIgnoresEnabled = pref.getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED);
		if (!isIgnoresEnabled) {
			return null;
		}

		boolean showingOpen = pref.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES);
		if (!showingOpen) {
			return new InfoTreeNode(ISnykToolView.OPEN_ISSUES_FILTERED_BUT_AVAILABLE);
		}

		return null;
	}

	private InfoTreeNode getNoIssueViewOptionsSelectedTreeNodeForCodeSecurity() {
		var pref = Preferences.getInstance();
		boolean isIgnoresEnabled = pref.getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED);
		if (!isIgnoresEnabled) {
			return null;
		}

		boolean showingOpen = pref.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES);
		if (!showingOpen) {
			return new InfoTreeNode(ISnykToolView.OPEN_ISSUES_FILTERED_BUT_AVAILABLE);
		}

		boolean showingIgnored = pref.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES);
		if (!showingIgnored) {
			return new InfoTreeNode(ISnykToolView.IGNORED_ISSUES_FILTERED_BUT_AVAILABLE);
		}

		return null;
	}

	private InfoTreeNode getFixableIssuesNode(long fixableIssueCount) {
		return new InfoTreeNode(fixableIssueCount > 0 ? ("⚡️ " + fixableIssueCount + " issue" + (fixableIssueCount == 1 ? "" : "s") + " can be fixed automatically") : "There are no issues automatically fixable.");
	}

	private InfoTreeNode getFixableIssuesNodeForCodeSecurity(long fixableIssueCount) {
		var pref = Preferences.getInstance();
		boolean isIgnoresEnabled = pref.getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED);
		boolean showingOpen = pref.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES);
		if (isIgnoresEnabled && !showingOpen) {
			return null;
		}
		return getFixableIssuesNode(fixableIssueCount);
	}

	@JsonNotification(value = LsConstants.SNYK_PUBLISH_DIAGNOSTICS_316)
	public void publishDiagnostics316(PublishDiagnostics316Param param) {
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

		populateIssueCache(param, filePath);
	}

	private void populateFileAndIssueNodes(ProductTreeNode productTreeNode, SnykIssueCache issueCache) {
		var cacheHashMap = issueCache.getCacheByDisplayProduct(productTreeNode.getProduct());
		List<Issue> issuesList = new ArrayList<>();
		for (var kv : cacheHashMap.entrySet()) {
			var fileName = kv.getKey();
			issuesList.clear(); // Clear the list instead of creating a new one
			issuesList.addAll(kv.getValue());

			if (issuesList.isEmpty())
				continue;

			FileTreeNode fileNode = new FileTreeNode(fileName); // NOPMD
			toolView.addFileNode(productTreeNode, fileNode);
			for (Issue issue : issuesList) {
				toolView.addIssueNode(fileNode, new IssueTreeNode(issue)); // NOPMD
			}
		}
	}

	private void populateIssueCache(PublishDiagnostics316Param param, String filePath) {
		var issueCache = getIssueCache(filePath);
		Diagnostic316[] diagnostics = param.getDiagnostics();
		if (diagnostics == null || diagnostics.length == 0) {
			issueCache.removeAllIssuesForPath(filePath);
			return;
		}
		var source = diagnostics[0].getSource();
		if (StringUtils.isEmpty(source)) {
			return;
		}
		var snykProduct = LSP_SOURCE_TO_SCAN_PARAMS.get(source);
		List<Issue> issueList = new ArrayList<>();
		var isIgnoresEnabled = Preferences.getInstance().getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED);
		for (var diagnostic : diagnostics) {
			if (diagnostic.getData() == null) {
				continue;
			}
			if (!isIgnoresEnabled && diagnostic.getData().isIgnored()) {
				toggleIgnores(true);
				isIgnoresEnabled = true;
			}
			issueList.add(diagnostic.getData());
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
		default:
			break;
		}
	}

	public void reportAnalytics(AbstractTask event) {
		try {
			var eventString = om.writeValueAsString(event);
			executeCommand(LsConstants.COMMAND_REPORT_ANALYTICS, List.of(eventString));
		} catch (Exception e) {
			SnykLogger.logError(e);
		}

	}

	@Override
	public CompletableFuture<Void> createProgress(WorkDoneProgressCreateParams params) {
		this.progressManager.addProgress(params.getToken().getLeft());
		return super.createProgress(params);
	}

	@Override
	public void notifyProgress(final ProgressParams params) {
		if (params.getValue() == null) {
			return;
		}
		WorkDoneProgressNotification progressNotification = params.getValue().getLeft();
		if (progressNotification != null && progressNotification.getKind() == WorkDoneProgressKind.end) {
			this.progressManager.removeProgress(params.getToken().getLeft());
		}
		super.notifyProgress(params);
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
			var result = executeCommand(LsConstants.COMMAND_GET_ACTIVE_USER, new ArrayList<>());
			// we don't wait forever, and if we can't get a user name (refresh
			// token), we're
			// done.
			try {
				result.get(timeout - 1, TimeUnit.SECONDS);
			} catch (InterruptedException | ExecutionException | TimeoutException e) {
				Thread.currentThread().interrupt();
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

	public void clearCache() {
		List<IProject> openProjects = ResourceUtils.getAccessibleTopLevelProjects();
		for (IProject iProject : openProjects) {
			IssueCacheHolder.getInstance().getCacheInstance(iProject).clearAll();
			try {
				iProject.deleteMarkers(MARKER_TYPE, true, IResource.DEPTH_INFINITE);
			} catch (CoreException e) {
				SnykLogger.logError(e);
			}
		}

	}

	@JsonRequest(value = "workspace/snyk.sdks")
	public CompletableFuture<List<LsSdk>> getSdks(WorkspaceFolder workspaceFolder) {
		return CompletableFuture.supplyAsync(() -> {
			List<LsSdk> sdks = new SdkHelper().getSdk(workspaceFolder);
			for (LsSdk lsSdk : sdks) {
				SnykLogger.logInfo("determined sdk: " + lsSdk);
			}
			return sdks;
		});
	}

	public void setToolWindow(ISnykToolView toolView) {
		this.toolView = toolView;
	}

	public void setLs(LanguageServer ls) {
		this.ls = ls;
	}

	public void setProgressMgr(ProgressManager progressMgr) {
		this.progressManager = progressMgr;
	}

	public ProgressManager getProgressManager() {
		return this.progressManager;
	}
}
