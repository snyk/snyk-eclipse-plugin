package io.snyk.eclipse.plugin.properties;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;

import com.google.gson.Gson;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfigsParam;

public class FolderConfigs {
	protected static FolderConfigs instance;
	private static final IEclipsePreferences instancePreferences = InstanceScope.INSTANCE.getNode(Activator.PLUGIN_ID);
	private static final Gson gson = new Gson();

	private FolderConfigs() {
	}

	public static synchronized FolderConfigs getInstance() {
		if (instance == null) {
			instance = new FolderConfigs();
		}
		return instance;
	}

	public void addFolderConfig(FolderConfig folderConfig) {
		var folderPath = Paths.get(folderConfig.getFolderPath());
		persist(folderPath, folderConfig);
	}

	public List<String> getLocalBranches(Path projectPath) {
		return getFolderConfig(projectPath).getLocalBranches();
	}
	
	public void setBaseBranch(Path projectPath, String newBaseBranch) {
		var folderConfig = getFolderConfig(projectPath);
		folderConfig.setBaseBranch(newBaseBranch);
		persist(projectPath.normalize(), folderConfig);
	}

	private void persist(Path path, FolderConfig folderConfig) {
		String updatedJson = gson.toJson(folderConfig);
		instancePreferences.put(path.toString(), updatedJson);
		try {
			instancePreferences.flush();
		} catch (Exception e) {
			SnykLogger.logError(e);
		}
	}

	public String getBaseBranch(Path projectPath) {
		return getFolderConfig(projectPath).getBaseBranch();
	}

	public void addAll(List<FolderConfig> folderConfigs) {
		for (FolderConfig folderConfig : folderConfigs) {
			addFolderConfig(folderConfig);
		}
	}

	public FolderConfigsParam updateFolderConfigs() {
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
			persist(path, folderConfig);
			folderConfigs.add(folderConfig);
		}
		
		FolderConfigsParam param = new FolderConfigsParam(folderConfigs);
		param.setFolderConfigs(folderConfigs);
		return param;
	}

	/**
	 * Gets folder config for given path, if none exists it is created with defaults and returned
	 * @param folderPath the path of the folder (usually the project)
	 * @return a folder config (always)
	 */
	public FolderConfig getFolderConfig(Path folderPath) {
		Path path = folderPath.normalize();
		String json = instancePreferences.get(path.toString(), null);
		if (json == null) {
			SnykLogger.logInfo("No valid configuration for path: " + folderPath.toString());
			FolderConfig folderConfig = new FolderConfig(path.toString(), null, new ArrayList<>(),
					new ArrayList<>());
			persist(path, folderConfig);
			return folderConfig;
		}
		return gson.fromJson(json, FolderConfig.class);
	}

	public static void setInstance(FolderConfigs folderConfigs) {
		instance = folderConfigs;
	}
}