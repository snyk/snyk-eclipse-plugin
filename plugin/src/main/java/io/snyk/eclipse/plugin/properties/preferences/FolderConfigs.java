package io.snyk.eclipse.plugin.properties.preferences;

import java.util.List;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;

import com.google.gson.Gson;

import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;

public class FolderConfigs {
	private static FolderConfigs instance;
	private static final IEclipsePreferences preferenceState = InstanceScope.INSTANCE.getNode("io.snyk.eclipse.plugin");
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
		preferenceState.put(folderConfig.getFolderPath(), gson.toJson(folderConfig));
		try {
			preferenceState.flush();
		} catch (Exception e) {
			SnykLogger.logError(e);
		}
	}

	public List<String> getLocalBranches(String folderPath) {
		String json = preferenceState.get(folderPath, null);
		if (json != null) {
			FolderConfig folderConfig = gson.fromJson(json, FolderConfig.class);
			return folderConfig.getLocalBranches();
		}
		return List.of();
	}

	public void setBaseBranch(String folderPath, String newBaseBranch) {
		String json = preferenceState.get(folderPath, null);
		if (json == null) {
			SnykLogger.logInfo("No valid configuration for project: " + folderPath);
			return;
		}
		FolderConfig folderConfig = gson.fromJson(json, FolderConfig.class);
		folderConfig.setBaseBranch(newBaseBranch);
		String updatedJson = gson.toJson(folderConfig);
		preferenceState.put(folderPath, updatedJson);
		try {
			preferenceState.flush();
		} catch (Exception e) {
			SnykLogger.logError(e);
		}
	}

	public String getBaseBranch(String folderPath) {
		String json = preferenceState.get(folderPath, null);
		if (json == null)
			return null;
		FolderConfig folderConfig = gson.fromJson(json, FolderConfig.class);
		return folderConfig.getBaseBranch();
	}
	
	public void addAll(List<FolderConfig> folderConfigs) {
		for (FolderConfig folderConfig : folderConfigs) {
			addFolderConfig(folderConfig);
		}
	}
}