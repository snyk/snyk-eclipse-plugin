package io.snyk.languageserver;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.lsp4e.LanguageServerWrapper;
import org.eclipse.lsp4e.LanguageServersRegistry;
import org.eclipse.lsp4e.LanguageServersRegistry.LanguageServerDefinition;
import org.eclipse.lsp4e.LanguageServiceAccessor;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;

import com.google.gson.Gson;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;


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

		String cliPath = getCliPathOrThrow(prefs);
		verifyCliProtocolVersion(cliPath);

		List<String> commands = Lists.of(cliPath, "language-server", "-l", "info");
		String workingDir = SystemUtils.USER_DIR;
		setCommands(commands);
		setWorkingDirectory(workingDir);
		try {
			super.start();
		} catch (IOException e) {
			SnykLogger.logAndShow("Cannot start the Snyk CLI. Please check the CLI path: " + cliPath);
			throw e;
		}
	}

	static void verifyCliProtocolVersion(String cliPath) throws IOException {
		try {
			ProcessBuilder pb = new ProcessBuilder(cliPath, "language-server", "--protocolVersion");
			pb.redirectErrorStream(true);
			Process proc = pb.start();
			String output = new String(proc.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
			proc.waitFor(5, TimeUnit.SECONDS);
			int actual = Integer.parseInt(output);
			int expected = Integer.parseInt(io.snyk.languageserver.download.LsBinaries.REQUIRED_LS_PROTOCOL_VERSION);
			if (actual != expected) {
				String msg = "Snyk CLI protocol version mismatch: expected " + expected + ", got " + actual
						+ ". Please update the Snyk CLI.";
				SnykLogger.logAndShow(msg);
				throw new IOException(msg);
			}
		} catch (NumberFormatException | InterruptedException e) {
			SnykLogger.logError(e);
		}
	}

	/**
	 * Returns CLI path if binary exists, logs and throws IOException with helpful message if not.
	 */
	static String getCliPathOrThrow(Preferences prefs) throws IOException {
		String cliPath = prefs.getCliPath();
		File cliBinary = new File(cliPath);
		if (!cliBinary.exists()) {
			var sb = new StringBuilder(200);
			sb.append("Snyk CLI binary not found at: '").append(cliPath).append("'. Expand Details for more information.\n");
			if (prefs.isManagedBinaries()) {
				sb.append("'Manage Binaries Automatically' is enabled - check the Error Log for download details. \n");
			}
			sb.append("You can also specify a custom CLI path in Snyk Preferences.");
			String message = sb.toString();
			SnykLogger.logAndShowError(message);
			throw new IOException(message);
		}
		return cliPath;
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

	/**
	 * Restarts the Snyk language server wrapper(s) so the next start picks up
	 * fresh settings (CLI path, custom env, etc.).
	 *
	 * <p>Three cases the implementation handles:
	 * <ul>
	 * <li>One or more wrappers are alive and healthy → restart each.</li>
	 * <li>A wrapper exists but its previous startup failed (e.g. CLI path was
	 * empty at boot) → restart still works because
	 * {@link org.eclipse.lsp4e.LanguageServerWrapper#restart()} stops then
	 * starts, even from a failed state. lsp4e excludes failed wrappers from
	 * {@code getStartedWrappers}, so we also look them up per-project via
	 * {@code getLSWrapper}.</li>
	 * <li>No wrapper exists yet → call {@code startLanguageServer} which
	 * creates one.</li>
	 * </ul>
	 */
	public static void restartSnykLanguageServer() {
		try {
			LanguageServerDefinition definition = LanguageServersRegistry.getInstance()
					.getDefinition(LANGUAGE_SERVER_ID);
			if (definition == null) {
				SnykLogger.logInfo("Cannot restart Snyk LS: definition not registered");
				return;
			}

			Set<LanguageServerWrapper> candidates = new HashSet<>();

			for (LanguageServerWrapper wrapper : LanguageServiceAccessor.getStartedWrappers(null, true)) {
				if (wrapper.serverDefinition != null && LANGUAGE_SERVER_ID.equals(wrapper.serverDefinition.id)) {
					candidates.add(wrapper);
				}
			}

			// getStartedWrappers omits wrappers whose startup failed. Look them up
			// per workspace project so a never-successfully-started LS can still
			// be restarted after the offending setting (e.g. empty CLI path)
			// has been corrected.
			for (IProject project : ResourceUtils.getAccessibleTopLevelProjects()) {
				LanguageServerWrapper wrapper = LanguageServiceAccessor.getLSWrapper(project, definition);
				if (wrapper != null) {
					candidates.add(wrapper);
				}
			}

			if (candidates.isEmpty()) {
				startSnykLanguageServer();
			} else {
				for (LanguageServerWrapper wrapper : candidates) {
					wrapper.restart();
				}
			}

			// lsp4e (re)start is fire-and-forget — give it a few seconds to
			// stand the LS up, then prompt the user to restart Eclipse if it's
			// still not active. Without this, a stuck/failed restart leaves the
			// user with a silently dead LS.
			schedulePostRestartCheck();
		} catch (Exception e) { // NOPMD - lsp4e can throw various unchecked errors; log and continue
			SnykLogger.logError(e);
			promptUserToRestartEclipse();
		}
	}

	private static void schedulePostRestartCheck() {
		CompletableFuture.delayedExecutor(POST_RESTART_CHECK_DELAY_SECONDS, TimeUnit.SECONDS).execute(() -> {
			try {
				if (!isAnySnykWrapperActive()) {
					promptUserToRestartEclipse();
				}
			} catch (Exception e) { // NOPMD
				SnykLogger.logError(e);
			}
		});
	}

	private static boolean isAnySnykWrapperActive() {
		for (LanguageServerWrapper wrapper : LanguageServiceAccessor.getStartedWrappers(null, true)) {
			if (wrapper.serverDefinition != null
					&& LANGUAGE_SERVER_ID.equals(wrapper.serverDefinition.id)
					&& wrapper.isActive()
					&& !wrapper.startupFailed()) {
				return true;
			}
		}
		return false;
	}

	private static void promptUserToRestartEclipse() {
		SnykLogger.logAndShow(
				"Snyk: could not reload the language server automatically with the new CLI binary. "
				+ "Please restart Eclipse for the change to take effect.");
	}

	private static final int POST_RESTART_CHECK_DELAY_SECONDS = 5;

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

		try {
			var param = new LsConfigurationUpdater().buildConfigurationParam();
			// Pre-serialize to JsonElement so LSP4J embeds the JSON directly
			// instead of re-serializing through its own Gson instance, which
			// can drop fields from custom POJOs like ConfigSetting.
			return new Gson().toJsonTree(param);
		} catch (Exception e) { // NOPMD - broad catch prevents LS handshake abort on unexpected errors
			// Handle initialization errors gracefully - log and return null to allow LS to start
			SnykLogger.logError(e);
		}
		return null;
	}
}
