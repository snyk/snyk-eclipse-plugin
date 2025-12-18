package io.snyk.languageserver;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;

import org.eclipse.lsp4j.DidChangeConfigurationParams;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigs;
import io.snyk.eclipse.plugin.properties.IssueViewOptions;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.FilterSeverity;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;
import io.snyk.languageserver.protocolextension.messageObjects.Settings;

public class LsConfigurationUpdater {

	public void configurationChanged() {
		SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
		if (lc != null) {
			lc.ensureLanguageServerRunning();
			var languageServer = lc.getConnectedLanguageServer();
			var params = new DidChangeConfigurationParams();
			params.setSettings(getCurrentSettings());
			languageServer.getWorkspaceService().didChangeConfiguration(params);
		}
	}

	Settings getCurrentSettings() {
		Preferences preferences = Preferences.getInstance();
		String activateSnykOpenSource = preferences.getPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE,
				Boolean.TRUE.toString());
		String activateSnykCodeSecurity = preferences.getPref(Preferences.ACTIVATE_SNYK_CODE_SECURITY,
				Boolean.FALSE.toString());
		String activateSnykIac = preferences.getPref(Preferences.ACTIVATE_SNYK_IAC, Boolean.TRUE.toString());
		String insecure = preferences.getPref(Preferences.INSECURE_KEY, Boolean.FALSE.toString());
		String endpoint = preferences.getPref(Preferences.ENDPOINT_KEY, "");
		String additionalParams = preferences.getPref(Preferences.ADDITIONAL_PARAMETERS, "");
		String additionalEnv = preferences.getPref(Preferences.ADDITIONAL_ENVIRONMENT, "");
		String path = preferences.getPref(Preferences.PATH_KEY, "");
		IssueViewOptions issueViewOptions = new IssueViewOptions(
				preferences.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES, true),
				preferences.getBooleanPref(Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES, false));
		String sendErrorReports = preferences.getPref(Preferences.SEND_ERROR_REPORTS, "");
		String enableTelemetry = preferences.getPref(Preferences.ENABLE_TELEMETRY, Boolean.FALSE.toString());
		String organization = preferences.getPref(Preferences.ORGANIZATION_KEY, "");
		String manageBinariesAutomatically = preferences.getPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY,
				Boolean.TRUE.toString());
		String cliPath = preferences.getCliPath();
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
		String authenticationMethod = preferences.getPref(Preferences.AUTHENTICATION_METHOD, AuthConstants.AUTH_OAUTH2);

		String enableDeltaFindings = preferences.getPref(Preferences.ENABLE_DELTA, Boolean.FALSE.toString());

		Integer riskScoreThreshold = null;
		String riskScoreThresholdStr = preferences.getPref(Preferences.RISK_SCORE_THRESHOLD, "0");
		try {
			int value = Integer.parseInt(riskScoreThresholdStr);
			if (value > 0) {
				riskScoreThreshold = value;
			}
		} catch (NumberFormatException e) {
			// ignore, keep null
		}

		// only add folder configs that are initialized
		var folderConfigs = new ArrayList<FolderConfig>();
		for (var p : Collections.unmodifiableSet(FolderConfigs.LanguageServerConfigReceived)) {
			folderConfigs.add(FolderConfigs.getInstance().getFolderConfig(p));
		}

		FilterSeverity filterSeverity = new FilterSeverity(
				preferences.getBooleanPref(Preferences.FILTER_SHOW_CRITICAL, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_HIGH, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_MEDIUM, true),
				preferences.getBooleanPref(Preferences.FILTER_SHOW_LOW, true));

		return new Settings(activateSnykOpenSource, activateSnykCodeSecurity, activateSnykIac,
				insecure, endpoint, additionalParams, additionalEnv, path, issueViewOptions, sendErrorReports,
				enableTelemetry, organization, manageBinariesAutomatically, cliPath, token, integrationName,
				integrationVersion, automaticAuthentication, trustedFolders, enableTrustedFolderFeature, scanningMode,
				enableDeltaFindings, riskScoreThreshold, authenticationMethod, folderConfigs, filterSeverity);
	}
}
