package io.snyk.languageserver;

import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.lsp4e.LanguageServersRegistry;
import org.eclipse.lsp4e.LanguageServiceAccessor;
import org.eclipse.lsp4j.DidChangeConfigurationParams;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.WorkspaceService;

@SuppressWarnings("restriction")
public class LsConfigurationUpdater {

  public void configurationChanged(Preferences preferences) {
    var params = new DidChangeConfigurationParams();
    params.setSettings(getCurrentSettings(preferences));

    var definition =
      LanguageServersRegistry.getInstance().getDefinition(SnykStreamConnectionProvider.LANGUAGE_SERVER_ID);

    if (definition == null) {
      return;
    }
    for (IProject project : ResourcesPlugin.getWorkspace().getRoot().getProjects()) {
      if (project.isAccessible()) {
        try {
          var servers = LanguageServiceAccessor.getLanguageServers(project, null);
          for (LanguageServer ls : servers) {
            var currentDefinition = LanguageServiceAccessor.resolveServerDefinition(ls);
            if (currentDefinition.isEmpty() || !currentDefinition.get().id.equals(definition.id))
              continue;
            WorkspaceService workspaceService = ls.getWorkspaceService();
            workspaceService.didChangeConfiguration(params);
          }
        } catch (Exception e) {
          SnykLogger.logError(e);
        }
      }
    }
  }

  Settings getCurrentSettings(Preferences preferences) {
    String activateSnykOpenSource = preferences.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "true");
    String activateSnykCode = preferences.getPref(Preferences.ACTIVATE_SNYK_CODE, "true");
    String activateSnykIac = preferences.getPref(Preferences.ACTIVATE_SNYK_IAC, "true");
    String insecure = preferences.getPref(Preferences.INSECURE_KEY, "false");
    String endpoint = preferences.getPref(Preferences.ENDPOINT_KEY, "");
    String additionalParams = preferences.getPref(Preferences.ADDITIONAL_PARAMETERS, "");
    String additionalEnv = preferences.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "");
    String path = preferences.getPref(Preferences.PATH_KEY, "");
    return new Settings(activateSnykOpenSource, activateSnykCode, activateSnykIac, insecure, endpoint, additionalParams,
      additionalEnv, path);
  }

  @SuppressWarnings("unused") // getters for GSon serialization
  static class Settings {
    private final String activateSnykOpenSource;
    private final String activateSnykCode;
    private final String activateSnykIac;

    private final String insecure;
    private final String endpoint;
    private final String additionalParams;
    private final String additionalEnv;
    private final String path;

    public Settings(String activateSnykOpenSource, String activateSnykCode, String activateSnykIac, String insecure,
                    String endpoint, String additionalParams, String additionalEnv, String path) {
      this.activateSnykOpenSource = activateSnykOpenSource;
      this.activateSnykCode = activateSnykCode;
      this.activateSnykIac = activateSnykIac;
      this.insecure = insecure;
      this.endpoint = endpoint;
      this.additionalParams = additionalParams;
      this.additionalEnv = additionalEnv;
      this.path = path;
    }

    public String getPath() {
      return path;
    }

    public String getActivateSnykOpenSource() {
      return activateSnykOpenSource;
    }

    public String getActivateSnykCode() {
      return activateSnykCode;
    }

    public String getActivateSnykIac() {
      return activateSnykIac;
    }

    public String getInsecure() {
      return insecure;
    }

    public String getEndpoint() {
      return endpoint;
    }

    public String getAdditionalParams() {
      return additionalParams;
    }

    public String getAdditionalEnv() {
      return additionalEnv;
    }
  }
}
