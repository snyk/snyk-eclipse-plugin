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
import static io.snyk.eclipse.plugin.properties.preferences.Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES;
import static io.snyk.eclipse.plugin.properties.preferences.Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.CONGRATS_NO_ISSUES_FOUND;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.NODE_TEXT_SCANNING;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.NO_FIXABLE_ISSUES;
import static io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView.getPlural;

import java.io.File;
import java.net.URI;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
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
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressCancelParams;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.WorkDoneProgressEnd;
import org.eclipse.lsp4j.WorkDoneProgressKind;
import org.eclipse.lsp4j.WorkDoneProgressNotification;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.analytics.AbstractTask;
import io.snyk.eclipse.plugin.analytics.AnalyticsEventTask;
import io.snyk.eclipse.plugin.analytics.TaskProcessor;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.SnykView;
import io.snyk.eclipse.plugin.views.snyktoolview.FileTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.InfoTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.IssueTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.ProductTreeNode;
import io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.FeatureFlagConstants;
import io.snyk.languageserver.IssueCacheHolder;
import io.snyk.languageserver.LsCommandID;
import io.snyk.languageserver.LsConfigurationUpdater;
import io.snyk.languageserver.LsNotificationID;
import io.snyk.languageserver.ScanInProgressKey;
import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.SnykIssueCache;
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.protocolextension.messageObjects.Diagnostic316;
import io.snyk.languageserver.protocolextension.messageObjects.FeatureFlagStatus;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.PublishDiagnostics316Param;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.IssueSorter;

@SuppressWarnings("restriction")
public class SnykExtendedLanguageClient extends LanguageClientImpl {
	private static final String SNYK_CODE_CONSISTENT_IGNORES = "snykCodeConsistentIgnores";
	private ProgressManager progressManager = new ProgressManager(this);
	private final ObjectMapper om = new ObjectMapper();
	private TaskProcessor taskProcessor;
	private ISnykToolView toolView;
	// this field is for testing only
	private LanguageServer ls;
	private LsConfigurationUpdater configurationUpdater = new LsConfigurationUpdater();

	private static SnykExtendedLanguageClient instance = null;

	public SnykExtendedLanguageClient() {
		super();
		instance = this;
		om.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		registerPluginInstalledEventTask();
		registerRefreshFeatureFlagsTask();
		createIssueCaches();
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
		Preferences.getInstance().store(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED,
				Boolean.valueOf(enableConsistentIgnores).toString());

		updateIgnoresButtons();
	}

	private void updateIgnoresButtons() {
		PlatformUI.getWorkbench().getDisplay().asyncExec(() -> {
			var snykToolView = SnykStartup.getView();
			if (snykToolView != null)
				snykToolView.toggleIgnoresButtons();
		});

	}

	private void createIssueCaches() {
		var workspace = ResourcesPlugin.getWorkspace();
		IProject[] allProjects = workspace.getRoot().getProjects();
		List<IProject> openProjects = Arrays.stream(allProjects).filter(IProject::isOpen).collect(Collectors.toList());
		for (IProject iProject : openProjects) {
			IssueCacheHolder.getInstance().getCacheInstance(iProject);
		}
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

	public static SnykExtendedLanguageClient getInstance() {
		return instance; // we leave instantiation to LSP4e, no lazy construction here
	}

	public LanguageServer getConnectedLanguageServer() {
		if (this.ls != null) {
			return ls;
		}
		return super.getLanguageServer();
	}

	public void triggerScan(IWorkbenchWindow window) {
		CompletableFuture.runAsync(() -> {
			if (Preferences.getInstance().getAuthToken().isBlank()) {
				runSnykWizard();
			} else {
				openToolView();
				this.toolView.resetNode(this.toolView.getRoot());
				try {
					if (window == null) {
						executeCommand(LsCommandID.COMMAND_WORKSPACE_SCAN, new ArrayList<>());
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
						executeCommand(LsCommandID.COMMAND_WORKSPACE_FOLDER_SCAN,
								List.of(project.getLocation().toOSString()));
					}
				} catch (Exception e) {
					SnykLogger.logError(e);
				}
			}
		});
	}

	public void triggerAuthentication() {
		executeCommand(LsCommandID.COMMAND_LOGIN, new ArrayList<>());
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
		executeCommand(LsCommandID.COMMAND_TRUST_WORKSPACE_FOLDERS, new ArrayList<>());
	}

	public boolean getSastEnabled() {
		try {
			CompletableFuture<Object> lsSastSettings = executeCommand(LsCommandID.COMMAND_GET_SETTINGS_SAST_ENABLED,
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
					LsCommandID.COMMAND_GET_FEATURE_FLAG_STATUS, List.of(featureFlag));
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
		CompletableFuture<Object> issueDescription = executeCommand(LsCommandID.COMMAND_GENERATE_ISSUE_DESCRIPTION,
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

		if (!Preferences.getInstance().isTest()) {
			configurationUpdater.configurationChanged();
			refreshFeatureFlags();
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
		SnykIssueCache issueCache = null;
		if (!param.getFolderPath().isBlank()) {
			issueCache = IssueCacheHolder.getInstance().getCacheInstance(param.getFolderPath());
		}

		openToolView();

		Set<ProductTreeNode> affectedProductTreeNodes = getAffectedProductNodes(param.getProduct(),
				param.getFolderPath());

		switch (param.getStatus()) {
		case SCAN_STATE_IN_PROGRESS:
			scanState.setScanInProgress(inProgressKey, true);
			for (ProductTreeNode productTreeNode : affectedProductTreeNodes) {
				toolView.resetNode(productTreeNode);
			}
			break;
		case SCAN_STATE_SUCCESS:
			scanState.setScanInProgress(inProgressKey, false);
			for (ProductTreeNode productTreeNode : affectedProductTreeNodes) {
				productTreeNode.reset();
				addInfoNodes(productTreeNode, param.getFolderPath(), issueCache);
				populateFileAndIssueNodes(productTreeNode, param.getFolderPath(), issueCache);
			}
			break;
		case SCAN_STATE_ERROR:
			scanState.setScanInProgress(inProgressKey, false);
			// Show error state
			break;
		}
		setNodeState(param.getStatus(), affectedProductTreeNodes, issueCache);
	}

	private void openToolView() {
		// we don't want to use the UI in tests usually
		if (this.toolView != null || Preferences.getInstance().isTest()) {
			return;
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
	}

	private Set<ProductTreeNode> getAffectedProductNodes(String snykScanProduct, String folderPath) {
		Set<ProductTreeNode> affectedProductTreeNodes = new HashSet<>();
		var displayProduct = SCAN_PARAMS_TO_DISPLAYED.get(snykScanProduct);
		if (displayProduct != null) {
			ProductTreeNode productNode = toolView.getProductNode(displayProduct, folderPath);
			if (productNode != null) {
				affectedProductTreeNodes.add(productNode);
			}
		} else {
			ProductTreeNode productNode = toolView.getProductNode(DISPLAYED_CODE_SECURITY, folderPath);
			if (productNode != null) {
				affectedProductTreeNodes.add(productNode);
			}
			productNode = toolView.getProductNode(DISPLAYED_CODE_QUALITY, folderPath);
			if (productNode != null) {
				affectedProductTreeNodes.add(productNode);
			}
		}
		return affectedProductTreeNodes;
	}

	private void setNodeState(String status, Set<ProductTreeNode> affectedProductTreeNodes, SnykIssueCache cache) {
		if (affectedProductTreeNodes.isEmpty()) {
			return;
		}
		var nodeText = "";

		if (status.equals(SCAN_STATE_IN_PROGRESS)) {
			nodeText = NODE_TEXT_SCANNING;
			setProductNodeText(affectedProductTreeNodes, nodeText);
		} else if (status.equals(SCAN_STATE_ERROR)) {
			nodeText = ISnykToolView.NODE_TEXT_ERROR;
			setProductNodeText(affectedProductTreeNodes, nodeText);
		} else if (status.equals(SCAN_STATE_SUCCESS)) {
			for (ProductTreeNode productTreeNode : affectedProductTreeNodes) {
				var displayCounts = getCountsSuffix(productTreeNode, cache);
				setProductNodeText(Set.of(productTreeNode), displayCounts);
			}
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

		long totalCount = issueCache.getTotalCount(productNode.getProduct());
		long fixableCount = issueCache.getFixableCount(productNode.getProduct());
		long ignoredCount = issueCache.getIgnoredCount(productNode.getProduct());
		var pref = Preferences.getInstance();
		if (totalCount == 0) {
			toolView.addInfoNode(productNode, new InfoTreeNode(CONGRATS_NO_ISSUES_FOUND));
		} else {
			String text = "✋ " + totalCount + " issue" + getPlural(totalCount) + " found by Snyk";
			if (ignoredCount > 0) {
				text += ", " + ignoredCount + " ignored";
			}
			toolView.addInfoNode(productNode, new InfoTreeNode(text));
		}
		if (totalCount > 0 && fixableCount == 0) {
			toolView.addInfoNode(productNode, new InfoTreeNode(NO_FIXABLE_ISSUES));
		}

		if (totalCount > 0 && fixableCount > 0) {
			toolView.addInfoNode(productNode, new InfoTreeNode(
					"⚡️ " + fixableCount + " issue" + getPlural(fixableCount) + " can be fixed automatically"));
		}

		if (totalCount > 0 && ignoredCount == totalCount
				&& pref.getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED)
				&& pref.getBooleanPref(FILTER_IGNORES_SHOW_OPEN_ISSUES)) {
			toolView.addInfoNode(productNode,
					new InfoTreeNode(ISnykToolView.IGNORED_ISSUES_FILTERED_BUT_AVAILABLE));
		}

		if (totalCount > 0 && ignoredCount == 0
				&& pref.getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED)
				&& pref.getBooleanPref(FILTER_IGNORES_SHOW_IGNORED_ISSUES)) {
			toolView.addInfoNode(productNode, new InfoTreeNode(ISnykToolView.OPEN_ISSUES_FILTERED_BUT_AVAILABLE));
		}
	}

	@JsonNotification(value = LsNotificationID.SNYK_PUBLISH_DIAGNOSTICS_316)
	public CompletableFuture<Void> publishDiagnostics316(PublishDiagnostics316Param param) {
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

			populateIssueCache(param, filePath);
		});
	}

	private void populateFileAndIssueNodes(ProductTreeNode productTreeNode, String folderPath, SnykIssueCache issueCache) {
		var cacheHashMap = Collections.unmodifiableMap(issueCache.getCacheByDisplayProduct(productTreeNode.getProduct()));
		for (var kv : cacheHashMap.entrySet()) {
			var fileName = kv.getKey();
			var issues = kv.getValue();
			if(issues.isEmpty())
				continue;
			issues = IssueSorter.sortIssuesBySeverity(issues);
			FileTreeNode fileNode = new FileTreeNode(fileName);
			toolView.addFileNode(productTreeNode, fileNode);
			for (Issue issue : issues) {
				toolView.addIssueNode(fileNode, new IssueTreeNode(issue));
			}
		}
	}

    private Collection<Issue> filterIgnoredIssues(Collection<Issue> issueList)
    {
    	final boolean includeIgnoredIssues;
    	final boolean includeOpenedIssues;

    	if (Preferences.getInstance().getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED)) {
    	    includeOpenedIssues = Preferences.getInstance().getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES);
    	    includeIgnoredIssues = Preferences.getInstance().getBooleanPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES);
    	} else {
    	    includeOpenedIssues = true;
    	    includeIgnoredIssues = true;
    	}

    	return issueList.stream()
    	    .filter(it -> it.isVisible(includeIgnoredIssues, includeOpenedIssues))
    	    .toList();

    }

	private void populateIssueCache(PublishDiagnostics316Param param, String filePath) {
		var issueCache = getIssueCache(filePath);
		Diagnostic316[] diagnostics = param.getDiagnostics();
		if (diagnostics.length == 0) {
			issueCache.removeAllIssuesForPath(filePath);
			return;
		}
		var source = diagnostics[0].getSource();
		if (StringUtils.isEmpty(source)) {
			return;
		}
		var snykProduct = LSP_SOURCE_TO_SCAN_PARAMS.get(source);
		List<Issue> issueList = new ArrayList<>();

		for (var diagnostic : diagnostics) {
			if (diagnostic.getData() == null) {
				continue;
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
		}
	}

	public void reportAnalytics(AbstractTask event) {
		try {
			var eventString = om.writeValueAsString(event);
			executeCommand(LsCommandID.COMMAND_REPORT_ANALYTICS, List.of(eventString));
		} catch (Exception e) {
			SnykLogger.logError(e);
		}

	}

	@Override
	public CompletableFuture<Void> createProgress(WorkDoneProgressCreateParams params) {
		this.progressManager.addProgress(params.getToken().getLeft());
		return super.createProgress(params);
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
			var result = executeCommand(LsCommandID.COMMAND_GET_ACTIVE_USER, new ArrayList<>());
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

	public void clearCache() {
		var workspace = ResourcesPlugin.getWorkspace();
		IProject[] allProjects = workspace.getRoot().getProjects();
		List<IProject> openProjects = Arrays.stream(allProjects).filter(IProject::isOpen).collect(Collectors.toList());
		for (IProject iProject : openProjects) {
			IssueCacheHolder.getInstance().getCacheInstance(iProject).clearAll();
		}

	}

	public void setProgressMgr(ProgressManager progressMgr) {
		this.progressManager = progressMgr;
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
	 * This cancels a progress in language server.
	 *
	 * @param token
	 */
	public void cancelProgress(String token) {
		// call language server to cancel
		var workDoneProgressCancelParams = new WorkDoneProgressCancelParams();
		workDoneProgressCancelParams.setToken(token);
		getConnectedLanguageServer().cancelProgress(workDoneProgressCancelParams);

		// call notify progress with end message
		var progressParam = getEndProgressParam(token);
		super.notifyProgress(progressParam);
	}

	ProgressParams getEndProgressParam(String token) {
		WorkDoneProgressEnd workDoneProgressEnd = new WorkDoneProgressEnd();
		workDoneProgressEnd.setMessage("Operation canceled.");
		Either<WorkDoneProgressNotification, Object> value = Either.forLeft(workDoneProgressEnd);
		Either<String, Integer> tokenEither = Either.forLeft(token);

		var progressParam = new ProgressParams(tokenEither, value);
		return progressParam;
	}

	public ProgressManager getProgressManager() {
		return this.progressManager;
	}

	public void setLs(LanguageServer ls) {
		this.ls = ls;
	}
}
