package io.snyk.languageserver;

import java.io.IOException;
import java.net.URI;
import java.util.List;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.lsp4e.LanguageServersRegistry;
import org.eclipse.lsp4e.LanguageServersRegistry.LanguageServerDefinition;
import org.eclipse.lsp4e.LanguageServiceAccessor;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.messageObjects.Settings;

@SuppressWarnings("restriction")
public class SnykLanguageServer extends ProcessStreamConnectionProvider implements StreamConnectionProvider {
	public static final String LANGUAGE_SERVER_ID = "io.snyk.languageserver";
	private final LsRuntimeEnvironment runtimeEnvironment;

	public SnykLanguageServer() {
		runtimeEnvironment = new LsRuntimeEnvironment();
	}

	@Override
	public void start() throws IOException {
		var prefs = Preferences.getInstance();
		waitForInit();
		List<String> commands = Lists.of(prefs.getCliPath(), "language-server", "-l", "info");
		String workingDir = SystemUtils.USER_DIR;
		setCommands(commands);
		setWorkingDirectory(workingDir);
		try {
			super.start();
		} catch (IOException e) {
			SnykLogger.logAndShow("Cannot start the Snyk CLI. Please check the CLI path.");
		}
	}

	public static void waitForInit() {
		if (Preferences.getInstance().isTest())
			return;
		waitForDownload();
		waitForSecurePreferencesToBeReady();
	}

	private static void waitForSecurePreferencesToBeReady() {
		while (!Preferences.getInstance().isSecureStorageReady()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
	}

	private static void waitForDownload() {
		while (SnykStartup.isDownloading()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
	}

	public static void startSnykLanguageServer() {
		LanguageServerDefinition definition = LanguageServersRegistry.getInstance()
				.getDefinition(SnykLanguageServer.LANGUAGE_SERVER_ID);
		LanguageServiceAccessor.startLanguageServer(definition);
	}

	@Override
	protected ProcessBuilder createProcessBuilder() {
		var pb = super.createProcessBuilder();
		runtimeEnvironment.updateEnvironment(pb.environment());
		return pb;
	}

	@Override
	public Object getInitializationOptions(URI rootUri) {
		if (!Preferences.getInstance().isTest()) {
			waitForInit();
		}

		Settings currentSettings = null;
		try {
			currentSettings = new LsConfigurationUpdater().getCurrentSettings();
		} catch (IllegalStateException | IllegalArgumentException e) {
			// Handle initialization errors gracefully - log and return null to allow LS to start
			SnykLogger.logError(e);
		}
		return currentSettings;
	}
}
