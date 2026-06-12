package io.snyk.languageserver.protocolextension;

import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_ERROR;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_IN_PROGRESS;
import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_SUCCESS;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Objects;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.Range;
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
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.analytics.AbstractTask;
import io.snyk.eclipse.plugin.analytics.AnalyticsEventTask;
import io.snyk.eclipse.plugin.analytics.TaskProcessor;
import io.snyk.eclipse.plugin.preferences.HTMLSettingsPreferencePage;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.CommandHandler;
import io.snyk.languageserver.FeatureFlagConstants;
import io.snyk.languageserver.LsConfigurationUpdater;
import io.snyk.languageserver.LsConstants;
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.protocolextension.messageObjects.FeatureFlagStatus;
import io.snyk.languageserver.protocolextension.messageObjects.ConfigSetting;
import io.snyk.languageserver.protocolextension.messageObjects.LspConfigurationParam;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.LsSdk;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;
import io.snyk.languageserver.protocolextension.messageObjects.SummaryPanelParams;
import io.snyk.languageserver.LsSettingsRegistry;
import io.snyk.languageserver.protocolextension.messageObjects.TreeViewParams;

@SuppressWarnings({"restriction", "PMD.AvoidCatchingGenericException"})
public class SnykExtendedLanguageClient extends LanguageClientImpl {
	private static final String FILE_SCHEME = "file";

	private ProgressManager progressManager = new ProgressManager(this);
	private final ObjectMapper om = new ObjectMapper();
	private TaskProcessor taskProcessor;
	private volatile ISnykToolView toolView;
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
			this.toolView.refreshDeltaReference();
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
			if (!Preferences.getInstance().isAuthenticated()) {
				SnykWizard.createAndLaunch();
			} else {
				var folderConfigSettings = FolderConfigSettings.getInstance();
				updateConfiguration();
				openToolView();
				try {
					if (projectPath != null) {
						if (folderConfigSettings.isConfigured(projectPath.toString())) {
							executeCommand(LsConstants.COMMAND_WORKSPACE_FOLDER_SCAN, List.of(projectPath.toString()));
						}
						return;
					}
					if (!folderConfigSettings.getAll().isEmpty()) {
						executeCommand(LsConstants.COMMAND_WORKSPACE_SCAN, new ArrayList<>());
					}
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

	public CompletableFuture<Object> triggerAuthentication() {
		return executeCommand(LsConstants.COMMAND_LOGIN, new ArrayList<>());
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

	public void sendCodeFixDiffsCommand(String issueID) {
		executeCommand(LsConstants.COMMAND_CODE_FIX_DIFFS, List.of(issueID));
	}

	public void sendCodeApplyAiFixEditCommand(String fixId) {
		executeCommand(LsConstants.COMMAND_CODE_FIX_APPLY_AI_EDIT, List.of(fixId));
	}

	public void submitIgnoreRequestCommands(String workflow, String issueId, String ignoreType, String ignoreReason,
			String ignoreExpirationDate) {
		executeCommand(LsConstants.COMMAND_SUBMIT_IGNORE_REQUEST,
				List.of(workflow, issueId, ignoreType, ignoreReason, ignoreExpirationDate));
	}

	@JsonNotification(value = LsConstants.SNYK_HAS_AUTHENTICATED)
	public void hasAuthenticated(HasAuthenticatedParam param) {
		var prefs = Preferences.getInstance();

		var oldToken = prefs.getAuthToken();
		var oldApi = prefs.getEndpoint();

		String newToken = param.getToken();
		boolean differentToken = !Objects.equals(newToken, oldToken);
		boolean differentApi = param.getApiUrl() != null && !param.getApiUrl().isBlank() && !param.getApiUrl().equals(oldApi);

		// Update UIs first, then persist to storage (avoids race conditions)
		if (differentToken) {
			HTMLSettingsPreferencePage.notifyAuthTokenChanged(newToken, param.getApiUrl());
		}

		if (differentApi) {
			prefs.store(Preferences.ENDPOINT_KEY, param.getApiUrl());
		}

		if (differentToken) {
			prefs.store(Preferences.AUTH_TOKEN_KEY, newToken);
		}

		if (!Preferences.getInstance().isTest()) {
			configurationUpdater.configurationChanged();
			refreshFeatureFlags();
		}

		if (differentToken && !newToken.isBlank()) {
			if (Preferences.getInstance().getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC)) {
				triggerScan(null);
			}
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
		openToolView();
		if (this.toolView != null) {
			this.toolView.refreshBrowser(param.getStatus());
		}
	}

	@JsonNotification(value = LsConstants.SNYK_CONFIGURATION)
	public void snykConfiguration(LspConfigurationParam param) {
		try {
			if (param == null) {
				return;
			}
			int settingsCount = param.getSettings() != null ? param.getSettings().size() : 0;
			int folderCount = param.getFolderConfigs() != null ? param.getFolderConfigs().size() : 0;
			SnykLogger.logInfo("$/snyk.configuration received: settings=" + settingsCount + ", folders=" + folderCount);

			if (param.getSettings() != null) {
				persistGlobalSettings(param.getSettings());
			}

			if (param.getFolderConfigs() != null) {
				var folderConfigSettings = FolderConfigSettings.getInstance();
				folderConfigSettings.addAll(param.getFolderConfigs());
				if (Preferences.getInstance().getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC)) {
					triggerScan(null);
				}
				if (this.toolView != null) {
					this.toolView.refreshDeltaReference();
				}
			}
		} catch (Exception e) {
			SnykLogger.logError(e);
		}
	}

	private void persistGlobalSettings(Map<String, ConfigSetting> settings) {
		var prefs = Preferences.getInstance();
		for (var entry : settings.entrySet()) {
			try {
				var registryEntry = LsSettingsRegistry.BY_LS_KEY.get(entry.getKey());
				if (registryEntry == null || registryEntry.prefKey == null || registryEntry.encrypted) {
					continue;
				}
				var setting = entry.getValue();
				if (setting.getValue() != null) {
					prefs.store(registryEntry.prefKey, registryEntry.inboundDeserializer.apply(setting.getValue()));
				}
			} catch (Exception e) {
				SnykLogger.logError(e);
			}
		}
	}

	@JsonNotification(value = LsConstants.SNYK_SCAN_SUMMARY)
	public void updateSummaryPanel(SummaryPanelParams summary) {
		CompletableFuture.runAsync(() -> {
			openToolView();
			if (this.toolView != null) {
				this.toolView.updateSummary(summary.getSummary());
			}
		});
	}

	@JsonNotification(value = LsConstants.SNYK_TREE_VIEW)
	public void snykTreeView(TreeViewParams params) {
		if (params == null || params.getTreeViewHtml() == null) {
			return;
		}
		String html = params.getTreeViewHtml();
		if (this.toolView != null) {
			this.toolView.updateTreeViewHtml(html);
			return;
		}
		CompletableFuture.runAsync(() -> {
			openToolView();
			if (this.toolView != null) {
				this.toolView.updateTreeViewHtml(html);
			}
		});
	}

	@Override
	public CompletableFuture<ShowDocumentResult> showDocument(ShowDocumentParams params) {
		URI uri;
		try {
			uri = new URI(params.getUri());
		} catch (URISyntaxException e) {
			SnykLogger.logError(e);
			return CompletableFuture.completedFuture(new ShowDocumentResult(false));
		}

		SnykShowFixUriDetails uriDetails = SnykShowFixUriDetails.fromURI(uri);

		if ("snyk".equals(uriDetails.scheme()) && !uriDetails.isValid()) {
			SnykLogger.logInfo(String.format("Unsupported snyk URI: action=%s, product=%s",
					uriDetails.action(), uriDetails.product()));
			return CompletableFuture.completedFuture(new ShowDocumentResult(false));
		}

		if (uriDetails.isValid()) {
			String issueId = uriDetails.issueId();
			String product = uriDetails.product();
			return CompletableFuture.supplyAsync(() -> new ShowDocumentResult(selectIssueNode(issueId, product)));
		} else if (FILE_SCHEME.equals(uriDetails.scheme())) {
			return openFileInEclipse(uri, params.getSelection());
		} else {
			return super.showDocument(params);
		}
	}

	private boolean selectIssueNode(String issueId, String product) {
		openToolView();
		ISnykToolView view = this.toolView;
		if (view == null) return false;
		view.selectTreeNode(issueId, product);
		return true;
	}

	// Opens a file:// URI inside Eclipse, forcing the default text editor for
	// unknown file types to avoid routing through macOS LaunchServices (e.g. Windsurf).
	private CompletableFuture<ShowDocumentResult> openFileInEclipse(URI uri, Range range) {
		if (Preferences.getInstance().isTest()) {
			return CompletableFuture.completedFuture(new ShowDocumentResult(false));
		}
		return CompletableFuture.supplyAsync(() -> {
			boolean[] success = { false };
			Display.getDefault().syncExec(() -> {
				try {
					IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
					if (window == null) {
						return;
					}
					IWorkbenchPage page = window.getActivePage();
					if (page == null) {
						return;
					}
					IEditorPart editor = openEditorForUri(page, uri);
					if (editor == null) {
						return;
					}
					if (range != null && editor instanceof ITextEditor) {
						revealRange((ITextEditor) editor, range);
					}
					success[0] = true;
				} catch (Exception e) {
					SnykLogger.logError(e);
				}
			});
			return new ShowDocumentResult(success[0]);
		});
	}

	private IEditorPart openEditorForUri(IWorkbenchPage page, URI uri) throws PartInitException, CoreException {
		URI normalizedUri = uri.normalize();
		if (normalizedUri.getPath() != null && normalizedUri.getPath().contains("..")) {
			SnykLogger.logInfo("Rejected URI with path traversal: " + normalizedUri);
			return null;
		}
		String editorId = resolveInternalEditorId(normalizedUri);
		IFile workspaceFile = LSPEclipseUtils.getFileHandle(normalizedUri.toString());
		if (workspaceFile != null && workspaceFile.exists()) {
			return page.openEditor(new org.eclipse.ui.part.FileEditorInput(workspaceFile), editorId);
		}
		IFileStore fileStore = EFS.getStore(normalizedUri);
		return page.openEditor(new FileStoreEditorInput(fileStore), editorId);
	}

	private String resolveInternalEditorId(URI uri) {
		try {
			IEditorDescriptor descriptor = IDE.getEditorDescriptor(uri.getPath(), true, false);
			if (descriptor != null && !descriptor.isOpenExternal()) {
				return descriptor.getId();
			}
		} catch (PartInitException e) {
			SnykLogger.logError(e);
		}
		return "org.eclipse.ui.DefaultTextEditor";
	}

	private void revealRange(ITextEditor textEditor, Range range) {
		IDocumentProvider provider = textEditor.getDocumentProvider();
		if (provider == null) {
			return;
		}
		IDocument doc = provider.getDocument(textEditor.getEditorInput());
		if (doc == null) {
			return;
		}
		try {
			int startOffset = doc.getLineOffset(range.getStart().getLine()) + range.getStart().getCharacter();
			int endOffset = doc.getLineOffset(range.getEnd().getLine()) + range.getEnd().getCharacter();
			textEditor.selectAndReveal(startOffset, endOffset - startOffset);
		} catch (BadLocationException e) {
			SnykLogger.logError(e);
		}
	}

	private ISnykToolView openToolView() {
		// we don't want to use the UI in tests usually
		if (this.toolView != null || Preferences.getInstance().isTest()) {
			return null;
		}
		Display.getDefault().syncExec(() -> {
			IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			try {
				toolView = (ISnykToolView) activePage.showView(SnykToolView.ID, null, IWorkbenchPage.VIEW_VISIBLE);
			} catch (PartInitException e) {
				SnykLogger.logError(e);
				return;
			}
		});
		return toolView;
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
		public boolean reportFalsePositivesEnabled;
		public String org;
		public boolean autofixEnabled;
	}


	public static <T> T convertInstanceOfObject(Object o, Class<T> clazz) {
		try {
			return clazz.cast(o);
		} catch (ClassCastException e) {
			return null;
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

	public String getConfigHtml() {
		if (getConnectedLanguageServer() == null) {
			return null;
		}
		try {
			CompletableFuture<Object> configHtmlFuture = executeCommand(
					LsConstants.COMMAND_WORKSPACE_CONFIGURATION, new ArrayList<>());
			Object result = configHtmlFuture.get(10, TimeUnit.SECONDS);
			return result != null ? String.valueOf(result) : null;
		} catch (TimeoutException e) {
			SnykLogger.logInfo("Timeout getting configuration HTML from language server");
		} catch (Exception e) {
			SnykLogger.logInfo("Error getting configuration HTML: " + e.getMessage());
		}
		return null;
	}

	public void logout() {
		executeCommand(LsConstants.COMMAND_LOGOUT, new ArrayList<>());
	}
}
