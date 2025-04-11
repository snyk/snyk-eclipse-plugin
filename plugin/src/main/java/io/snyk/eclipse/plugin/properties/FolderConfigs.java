package io.snyk.eclipse.plugin.properties;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.IScopeContext;
import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;

public class FolderConfigs {
	protected static FolderConfigs instance;
	private static Map<String, FolderConfig> inMemoryConfigs = new HashMap<>();
	public static Boolean LanguageServerConfigReceived = false;

	private FolderConfigs() {
	}

	public static synchronized FolderConfigs getInstance() {
		if (instance == null) {
			instance = new FolderConfigs();
		}
		return instance;
	}

	public void addFolderConfig(FolderConfig folderConfig) {
		storeInMemory(Paths.get(folderConfig.getFolderPath()), folderConfig);
	}

	public List<String> getLocalBranches(Path projectPath) {
		return getFolderConfig(projectPath).getLocalBranches();
	}

	private void storeInMemory(Path path, FolderConfig folderConfig) {
		String folderPath = path.normalize().toString();
		inMemoryConfigs.put(folderPath, folderConfig);
	}

	public String getBaseBranch(Path projectPath) {
		return getFolderConfig(projectPath).getBaseBranch();
	}

	public void addAll(List<FolderConfig> folderConfigs) {
		for (FolderConfig folderConfig : folderConfigs) {
			addFolderConfig(folderConfig);
		}
	}

	public List<FolderConfig> getAll() {
		List<IProject> openProjects = ResourceUtils.getAccessibleTopLevelProjects();
		List<FolderConfig> folderConfigs = new ArrayList<>(openProjects.size());

		for (var project : openProjects) {
			Path path = ResourceUtils.getFullPath(project);
			//Linter does not like when new objects are created in loops, but here we do want to create new ProjectScopes in the loop.
			IScopeContext projectScope = new ProjectScope(project); //NOPMD, 
			var projectSettings = projectScope.getNode(Activator.PLUGIN_ID);
			String additionalParams = projectSettings.get(ProjectPropertyPage.SNYK_ADDITIONAL_PARAMETERS, "");
			var additionalParamsList = Arrays.asList(additionalParams.split(" "));
			var folderConfig = getFolderConfig(path);
			folderConfig.setAdditionalParameters(additionalParamsList);
			storeInMemory(path, folderConfig);
			folderConfigs.add(folderConfig);
		}

		return Collections.unmodifiableList(folderConfigs);
	}

	/**
	 * Gets folder config for given path, if none exists it is created with defaults and returned
	 * @param folderPath the path of the folder (usually the project)
	 * @return a folder config (always)
	 */
	public FolderConfig getFolderConfig(Path folderPath) {
		String path = folderPath.normalize().toString();

		FolderConfig folderConfig = inMemoryConfigs.get(path);
		if (folderConfig == null) {
			SnykLogger.logInfo("Did not find FolderConfig for path" + path + ", creating new one.");
			folderConfig = new FolderConfig(path);
		}
		return folderConfig;
	}

	public static void setInstance(FolderConfigs folderConfigs) {
		instance = folderConfigs;
	}

	public void setLanguageServerConfigReceived() {
		LanguageServerConfigReceived = true;
	}
}