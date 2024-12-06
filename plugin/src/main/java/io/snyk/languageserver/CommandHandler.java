package io.snyk.languageserver;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.ResourceBundle;
import java.util.concurrent.CompletableFuture;

import org.eclipse.core.resources.IProject;
import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.services.LanguageServer;

import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class CommandHandler {
	private LanguageServer ls;
	private static CommandHandler instance;

	public CommandHandler(LanguageServer ls) {
		this.ls = ls;
	}

	public static synchronized CommandHandler getInstance() {
		if (instance == null) {
			SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
			lc.ensureLanguageServerRunning();
			instance = new CommandHandler(lc.getConnectedLanguageServer());
		}
		return instance;
	}

	public CompletableFuture<Object> executeCommand(@NonNull String command, List<Object> args) {
		ExecuteCommandParams params = new ExecuteCommandParams(command, args);
		try {
			return ls.getWorkspaceService().executeCommand(params);
		} catch (Exception e) {
			SnykLogger.logError(e);
		}
		return CompletableFuture.completedFuture(null);
	}

	public CompletableFuture<Object> ignoreIssue(Issue issue) {
		Path workingDir = getWorkingDirectory(issue);
		List<Object> args = List.of(workingDir.toString(), "ignore", "--id=" + issue.additionalData().ruleId());
		return this.executeCommand(LsConstants.COMMAND_SNYK_CLI, args);
	}

	protected Path getWorkingDirectory(Issue issue) {
		IProject project = ResourceUtils.getProjectByPath(Paths.get(issue.filePath()));
		var workingDir = ResourceUtils.getFullPath(project);
		return workingDir;
	}

	public CompletableFuture<Object> monitorProject(IProject project) {
		Path workingDir = ResourceUtils.getFullPath(project);
		List<Object> args = List.of(workingDir.toString(), "monitor", "--all-projects");
		return this.executeCommand(LsConstants.COMMAND_SNYK_CLI, args);
	}

}
