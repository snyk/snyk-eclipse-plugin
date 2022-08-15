package io.snyk.languageserver;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;

import java.util.Collections;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.lsp4e.LanguageServersRegistry;
import org.eclipse.lsp4e.LanguageServiceAccessor;
import org.eclipse.lsp4j.DidChangeConfigurationParams;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.WorkspaceService;

@SuppressWarnings("restriction")
public class LsConfigurationUpdater {

  public void configurationChanged() {
    var params = new DidChangeConfigurationParams();
    params.setSettings(getCurrentSettings());

    var definition = LanguageServersRegistry.getInstance().getDefinition(SnykLanguageServer.LANGUAGE_SERVER_ID);

    if (definition == null) {
      return;
    }
    for (IProject project : ResourcesPlugin.getWorkspace().getRoot().getProjects()) {
      if (project.isAccessible()) {
        try {
          var servers = Collections.unmodifiableCollection(LanguageServiceAccessor.getLanguageServers(project, null));
          for (LanguageServer ls : servers) {
            var currentDefinition = LanguageServiceAccessor.resolveServerDefinition(ls);
            if (currentDefinition.isEmpty() || !currentDefinition.get().id.equals(definition.id)) continue;
            WorkspaceService workspaceService = ls.getWorkspaceService();
            workspaceService.didChangeConfiguration(params);
          }
        } catch (Exception e) {
          SnykLogger.logError(e);
        }
      }
    }
  }

  Settings getCurrentSettings() {
    Preferences preferences = Preferences.getInstance();
    String activateSnykOpenSource = preferences.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "true");
    String activateSnykCode = preferences.getPref(Preferences.ACTIVATE_SNYK_CODE, "false");
    String activateSnykIac = preferences.getPref(Preferences.ACTIVATE_SNYK_IAC, "true");
    String insecure = preferences.getPref(Preferences.INSECURE_KEY, "false");
    String endpoint = preferences.getPref(Preferences.ENDPOINT_KEY, "");
    String additionalParams = preferences.getPref(Preferences.ADDITIONAL_PARAMETERS, "");
    String additionalEnv = preferences.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "");
    String path = preferences.getPref(Preferences.PATH_KEY, "");
    String sendErrorReports = preferences.getPref(Preferences.SEND_ERROR_REPORTS, "");
    String enableTelemetry = preferences.getPref(Preferences.ENABLE_TELEMETRY, "false");
    String organization = preferences.getPref(Preferences.ORGANIZATION_KEY, "");
    String manageBinariesAutomatically = preferences.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "true");
    String cliPath = preferences.getPref(Preferences.CLI_PATH, "");
    String token = preferences.getPref(Preferences.AUTH_TOKEN_KEY, "");
    String integrationName = Activator.INTEGRATION_NAME;
    String integrationVersion = Activator.PLUGIN_VERSION;
    return new Settings(activateSnykOpenSource,
        activateSnykCode,
        activateSnykIac,
        insecure,
        endpoint,
        additionalParams,
        additionalEnv,
        path,
        sendErrorReports,
        enableTelemetry,
        organization,
        manageBinariesAutomatically,
        cliPath,
        token,
        integrationName,
        integrationVersion
        );
  }

  static class Settings {
    private final String activateSnykOpenSource;
    private final String activateSnykCode;
    private final String activateSnykIac;

    private final String insecure;
    private final String endpoint;
    private final String additionalParams;
    private final String additionalEnv;
    private final String path;
    private final String sendErrorReports;
    private final String enableTelemetry;
    private final String organization;
    private final String manageBinariesAutomatically;
    private final String cliPath;
    private final String token;
    private final String integrationName;
    private final String integrationVersion;

    public Settings(String activateSnykOpenSource, 
        String activateSnykCode, 
        String activateSnykIac, 
        String insecure,
        String endpoint, 
        String additionalParams, 
        String additionalEnv, 
        String path, 
        String sendErrorReports,
        String enableTelemetry, 
        String organization, 
        String manageBinariesAutomatically, String cliPath,
        String token, String integrationName, String integrationVersion) {
      this.activateSnykOpenSource = activateSnykOpenSource;
      this.activateSnykCode = activateSnykCode;
      this.activateSnykIac = activateSnykIac;
      this.insecure = insecure;
      this.endpoint = endpoint;
      this.additionalParams = additionalParams;
      this.additionalEnv = additionalEnv;
      this.path = path;
      this.sendErrorReports = sendErrorReports;
      this.enableTelemetry = enableTelemetry;
      this.organization = organization;
      this.manageBinariesAutomatically = manageBinariesAutomatically;
      this.cliPath = cliPath;
      this.token = token;
      this.integrationName = integrationName;
      this.integrationVersion = integrationVersion;
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

    public String getSendErrorReports() {
      return this.sendErrorReports;
    }

    public String getEnableTelemetry() {
      return this.enableTelemetry;
    }

    public String getOrganization() {
      return this.organization;
    }

    public String getManageBinariesAutomatically() {
      return this.manageBinariesAutomatically;
    }

    public String getCliPath() {
      return cliPath;
    }

    public String getToken() {
      return token;
    }

    public String getIntegrationName() {
      return integrationName;
    }

    public String getIntegrationVersion() {
      return integrationVersion;
    }
  }
}
