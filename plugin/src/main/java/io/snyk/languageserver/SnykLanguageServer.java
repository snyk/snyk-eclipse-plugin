package io.snyk.languageserver;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.lsp4e.LanguageServersRegistry;
import org.eclipse.lsp4e.LanguageServersRegistry.LanguageServerDefinition;
import org.eclipse.lsp4e.LanguageServiceAccessor;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

import com.google.gson.Gson;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
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
		String output = "";
		try {
			ProcessBuilder pb = new ProcessBuilder(cliPath, "language-server", "--protocolVersion");
			pb.redirectErrorStream(true);
			Process proc = pb.start();
			output = new String(proc.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
			proc.waitFor(5, TimeUnit.SECONDS);
			// Use first line only: some CLI builds print version + trailing warnings.
			String firstLine = output.split("\\n", 2)[0].trim();
			int actual = Integer.parseInt(firstLine);
			int expected = Integer.parseInt(io.snyk.languageserver.download.LsBinaries.REQUIRED_LS_PROTOCOL_VERSION);
			if (actual != expected) {
				String msg = "Snyk CLI protocol version mismatch: expected " + expected + ", got " + actual
						+ ". Please update the Snyk CLI.";
				SnykLogger.logAndShowError(msg);
				throw new IOException(msg);
			}
		} catch (NumberFormatException e) {
			SnykLogger.logDebug("verifyCliProtocolVersion: unexpected output: " + output);
			String msg = "Snyk CLI binary at '" + cliPath + "' is not compatible with this version of the Eclipse plugin. Please update the Snyk CLI.";
			SnykLogger.logAndShowError(msg);
			throw new IOException(msg);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			SnykLogger.logInfo("CLI protocol version check interrupted.");
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
	 * Asks the user whether to restart Eclipse so the new CLI binary takes
	 * effect, and triggers the restart on confirmation.
	 *
	 * <p>Restarting the language server in-process is not reliable because
	 * lsp4e caches the {@code StreamConnectionProvider} per definition and
	 * because a wrapper whose previous startup failed (e.g. CLI path was empty
	 * at boot) doesn't recover even after {@code wrapper.restart()}. The
	 * cheapest correct UX is to restart Eclipse, which picks the new CLI path
	 * up on the next normal startup path.
	 */
	public static void promptToRestartEclipseForNewCli() {
		Display display = PlatformUI.getWorkbench().getDisplay();
		display.asyncExec(() -> {
			boolean confirmed = MessageDialog.openQuestion(
					display.getActiveShell(),
					"Restart Eclipse",
					"The Snyk CLI binary has changed. Restart Eclipse now so the new CLI "
					+ "takes effect?");
			if (confirmed) {
				PlatformUI.getWorkbench().restart();
			}
		});
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
