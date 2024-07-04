package io.snyk.languageserver;

import java.io.File;

import io.snyk.languageserver.download.LsBinaries;
import org.apache.commons.lang3.SystemUtils;
import org.eclipse.lsp4j.DidChangeConfigurationParams;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

@SuppressWarnings("restriction")
public class LsConfigurationUpdater {

    public void configurationChanged() {
        var params = new DidChangeConfigurationParams();
        params.setSettings(getCurrentSettings());

        SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
        if (lc != null) {
            var languageServer = lc.getConnectedLanguageServer();
            languageServer.getWorkspaceService().didChangeConfiguration(params);
        }
    }

    Settings getCurrentSettings() {
        Preferences preferences = Preferences.getInstance();
        String activateSnykOpenSource = preferences.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, Boolean.TRUE.toString());
        String activateSnykCode = preferences.getPref(Preferences.ACTIVATE_SNYK_CODE, Boolean.FALSE.toString());
        String activateSnykIac = preferences.getPref(Preferences.ACTIVATE_SNYK_IAC, Boolean.TRUE.toString());
        String insecure = preferences.getPref(Preferences.INSECURE_KEY, Boolean.FALSE.toString());
        String endpoint = preferences.getPref(Preferences.ENDPOINT_KEY, "");
        String additionalParams = preferences.getPref(Preferences.ADDITIONAL_PARAMETERS, "");
        String additionalEnv = preferences.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "");
        String path = preferences.getPref(Preferences.PATH_KEY, "");
        String sendErrorReports = preferences.getPref(Preferences.SEND_ERROR_REPORTS, "");
        String enableTelemetry = preferences.getPref(Preferences.ENABLE_TELEMETRY, Boolean.FALSE.toString());
        String organization = preferences.getPref(Preferences.ORGANIZATION_KEY, "");
        String manageBinariesAutomatically = preferences.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY,
            Boolean.TRUE.toString());
        String cliPath = preferences.getPref(Preferences.CLI_PATH, "");
        String token = preferences.getPref(Preferences.AUTH_TOKEN_KEY, "");
        String integrationName = Activator.INTEGRATION_NAME;
        String integrationVersion = Activator.PLUGIN_VERSION;
        String automaticAuthentication = "false";
        String trustedFoldersString = preferences.getPref(Preferences.TRUSTED_FOLDERS);
        String[] trustedFolders = new String[0];
        if (trustedFoldersString != null && !trustedFoldersString.isBlank()) {
            trustedFolders = trustedFoldersString.split(File.pathSeparator);
        }
        String enableTrustedFolderFeature = Boolean.TRUE.toString();
        String scanningMode = preferences.getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC) ? "automatic" : "manual";
        Boolean useTokenAuth = preferences.getBooleanPref(Preferences.USE_TOKEN_AUTH, false);
        var authMethod = "oauth";
        if (useTokenAuth) {
        	authMethod = "token";
        } 
        return new Settings(activateSnykOpenSource, activateSnykCode, activateSnykIac, insecure, endpoint, additionalParams,
            additionalEnv, path, sendErrorReports, enableTelemetry, organization, manageBinariesAutomatically, cliPath,
            token, integrationName, integrationVersion, automaticAuthentication, trustedFolders, enableTrustedFolderFeature,
            scanningMode, authMethod);
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
        private final String automaticAuthentication;
        private final String[] trustedFolders;
        private final String enableTrustedFoldersFeature;
        private final String runtimeName = SystemUtils.JAVA_RUNTIME_NAME;
        private final String runtimeVersion = SystemUtils.JAVA_RUNTIME_VERSION;
        private final String osArch = SystemUtils.OS_ARCH;
        private final String osPlatform = SystemUtils.OS_NAME;
        private final String scanningMode;
        private final String requiredProtocolVersion = LsBinaries.REQUIRED_LS_PROTOCOL_VERSION;
        private final String authenticationMethod;

        public Settings(String activateSnykOpenSource, String activateSnykCode, String activateSnykIac, String insecure,
                        String endpoint, String additionalParams, String additionalEnv, String path, String sendErrorReports,
                        String enableTelemetry, String organization, String manageBinariesAutomatically, String cliPath, String token,
                        String integrationName, String integrationVersion, String automaticAuthentication, String[] trustedFolders,
                        String enableTrustedFoldersFeature, String scanningMode, String authMethod) {
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
            this.automaticAuthentication = automaticAuthentication;
            this.trustedFolders = trustedFolders;
            this.enableTrustedFoldersFeature = enableTrustedFoldersFeature;
            this.scanningMode = scanningMode;
            this.authenticationMethod = authMethod;
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

        public String getAutomaticAuthentication() {
            return automaticAuthentication;
        }

        public String[] getTrustedFolders() {
            return trustedFolders;
        }

        public String getEnableTrustedFoldersFeature() {
            return enableTrustedFoldersFeature;
        }

        public String getRuntimeName() {
            return runtimeName;
        }

        public String getRuntimeVersion() {
            return runtimeVersion;
        }

        public String getOsArch() {
            return osArch;
        }

        public String getOsPlatform() {
            return osPlatform;
        }

        public String getScanningMode() {
            return scanningMode;
        }

		public String getAuthenticationMethod() {
			return authenticationMethod;
		}

        public String getRequiredProtocolVersion() {
            return requiredProtocolVersion;
        }
    }
}
