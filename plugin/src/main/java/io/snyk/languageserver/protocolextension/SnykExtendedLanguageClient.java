package io.snyk.languageserver.protocolextension;

import java.io.File;
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

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.lsp4e.LanguageClientImpl;
import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressCreateParams;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.analytics.AbstractAnalyticsEvent;
import io.snyk.eclipse.plugin.analytics.AnalyticsEvent;
import io.snyk.eclipse.plugin.analytics.AnalyticsSender;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.SnykView;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.protocolextension.messageObjects.HasAuthenticatedParam;
import io.snyk.languageserver.protocolextension.messageObjects.SnykIsAvailableCliParams;
import io.snyk.languageserver.protocolextension.messageObjects.SnykTrustedFoldersParams;

@SuppressWarnings("restriction")
public class SnykExtendedLanguageClient extends LanguageClientImpl {
	private final ProgressManager progressMgr = new ProgressManager();
	private final ObjectMapper om = new ObjectMapper();
	private AnalyticsSender analyticsSender;
	
	private static SnykExtendedLanguageClient instance = null;
	// we overwrite the super-class field, so we can mock it

	public SnykExtendedLanguageClient() {
		super();
		instance = this;
		sendPluginInstalledEvent();
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
						executeCommand("snyk.workspace.scan", new ArrayList<>());
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
						executeCommand("snyk.workspaceFolder.scan", List.of(project.getLocation().toOSString()));
					}
				} catch (Exception e) {
					SnykLogger.logError(e);
				}
			}
		});
	}

	public void triggerAuthentication() {
		executeCommand("snyk.login", new ArrayList<>());
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
		executeCommand("snyk.trustWorkspaceFolders", new ArrayList<>());
	}

	public boolean getSastEnabled() {
		try {
			CompletableFuture<Object> lsSastSettings = executeCommand("snyk.getSettingsSastEnabled", new ArrayList<>());
			Object result;
			try {
				result = lsSastSettings.get(5, TimeUnit.SECONDS);
			} catch (TimeoutException e) {
				SnykLogger.logInfo("did not get a response for sast settings, disabling Snyk Code");
				return false;
			}

			ObjectMapper mapper = new ObjectMapper();
			mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
			SastSettings sastSettings = mapper.convertValue(result, SastSettings.class);
			return sastSettings != null ? sastSettings.sastEnabled : false;
		} catch (Exception e) {
			SnykLogger.logError(e);
		}

		return false;
	}

	@JsonNotification(value = "$/snyk.hasAuthenticated")
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

	@JsonNotification(value = "$/snyk.isAvailableCli")
	public void isAvailableCli(SnykIsAvailableCliParams param) {
		Preferences.getInstance().store(Preferences.CLI_PATH, param.getCliPath());
		enableSnykViewRunActions();
	}

	@JsonNotification(value = "$/snyk.addTrustedFolders")
	public void addTrustedPaths(SnykTrustedFoldersParams param) {
		var prefs = Preferences.getInstance();
		var storedTrustedPaths = prefs.getPref(Preferences.TRUSTED_FOLDERS, "");
		var trustedPaths = storedTrustedPaths.split(File.pathSeparator);
		var pathSet = new HashSet<>(Arrays.asList(trustedPaths));
		pathSet.addAll(Arrays.asList(param.getTrustedFolders()));
		Preferences.getInstance().store(Preferences.TRUSTED_FOLDERS, pathSet.stream().filter(s -> !s.isBlank())
				.map(s -> s.trim()).distinct().collect(Collectors.joining(File.pathSeparator)));
	}

	public void reportAnalytics(AbstractAnalyticsEvent event) {
		try {
			var eventString = om.writeValueAsString(event);
			executeCommand("snyk.reportAnalytics", List.of(eventString));
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
			var result = executeCommand("snyk.getActiveUser", new ArrayList<>());
			// we don't wait forever, and if we can't get a user name (refresh token), we're done.
			try {
				result.get(timeout-1, TimeUnit.SECONDS);
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
}
