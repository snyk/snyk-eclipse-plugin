package io.snyk.languageserver;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.lsp4j.ExecuteCommandParams;
import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.services.LanguageServer;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class CommandHandler {
	private LanguageServer ls;
	private static CommandHandler instance;
	private static final Set<String> allowedProducts = Set.of(ProductConstants.DISPLAYED_IAC,
			ProductConstants.DISPLAYED_OSS);

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
		String displayProduct = ProductConstants.FILTERABLE_ISSUE_TYPE_TO_DISPLAY.get(issue.filterableIssueType());
		if (!canBeIgnored(displayProduct))
			return CompletableFuture.completedFuture(null);

		Path workingDir = getWorkingDirectory(issue);
		String pathArg = null;
		String idArg = "--id=" + issue.additionalData().ruleId();
		
		List<Object> args = new ArrayList<>();
		args.add(workingDir.toString());
		args.add("ignore");

		if (issue.filterableIssueType().equals(ProductConstants.FILTERABLE_ISSUE_INFRASTRUCTURE_AS_CODE)) {
			Path relativePath = workingDir.relativize(Paths.get(issue.filePath()));
			var separator = " > ";
			pathArg = "--path=" + relativePath.toString() + separator
					+ issue.additionalData().path().stream().collect(Collectors.joining(separator));
			args.add(pathArg);
			idArg = "--id=" + issue.additionalData().publicId();
		}

		args.add(idArg);

		return this.executeCommand(LsConstants.COMMAND_SNYK_CLI, args);
	}

	protected Path getWorkingDirectory(Issue issue) {
		IProject project = ResourceUtils.getProjectByPath(Paths.get(issue.filePath()));
		if (project == null)
			return Path.of(".");
		var workingDir = ResourceUtils.getFullPath(project);
		return workingDir;
	}

	public CompletableFuture<Object> monitorProject(Path path) {
		List<Object> args = List.of(path.toString(), "monitor", "--all-projects");
		return this.executeCommand(LsConstants.COMMAND_SNYK_CLI, args);
	}

	/**
	 * checks if a product can be ignored
	 * 
	 * @param product the DISPLAY product from ProductConstants
	 * @return
	 */
	public boolean canBeIgnored(String product) {
		if (product == null)
			return false;
		return allowedProducts.contains(product);
	}

}
