package io.snyk.eclipse.plugin;

import static io.snyk.eclipse.plugin.utils.SnykLogger.logError;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.time.temporal.ChronoUnit;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.SnykView;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.LsRuntimeEnvironment;
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.download.HttpClientFactory;
import io.snyk.languageserver.download.LsBinaries;
import io.snyk.languageserver.download.LsDownloader;

public class SnykStartup implements IStartup {
	private static LsRuntimeEnvironment runtimeEnvironment;
	private SnykView snykView = null;
	private static SnykToolView snykToolView = null;
	private static boolean downloading = true;
	private static ILog logger;

	private static SnykStartup instance;

	@Override
	public void earlyStartup() {
		instance = this;
		if (logger == null) {
			logger = Platform.getLog(getClass());
		}
		runtimeEnvironment = new LsRuntimeEnvironment();
		Job initJob = new Job("Downloading latest CLI release...") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					logger.info("LS: Checking for needed download");
					if (downloadLS()) {
						monitor.beginTask("Downloading CLI", 100);
						logger.info("LS: Need to download");
						downloading = true;
						download(monitor);
					}
				} catch (Exception exception) {
					logError(exception);
				}
				downloading = false;
				monitor.subTask("Starting Snyk CLI in Language Server mode...");
				startLanguageServer();

				PlatformUI.getWorkbench().getDisplay().syncExec(() -> {
					Preferences prefs = Preferences.getInstance();
					if (prefs.getAuthToken().isBlank() && !prefs.isTest()) {
						monitor.subTask("Starting Snyk Wizard to configure initial settings...");
						SnykWizard wizard = new SnykWizard();
						WizardDialog dialog = new WizardDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell(),
								wizard);
						dialog.setBlockOnOpen(true);
						dialog.open();
					}
				});
				monitor.done();

				return Status.OK_STATUS;
			}

			private void startLanguageServer() {
				try {
					SnykLanguageServer.startSnykLanguageServer();
				} catch (RuntimeException e) {
					logError(e);
				}
			}
		};
		initJob.setPriority(Job.LONG);
		initJob.schedule();
	}

	public static SnykView getSnykView() {
		if (instance.snykView == null) {
			IWorkbench workbench = PlatformUI.getWorkbench();

			workbench.getDisplay().syncExec(() -> {
				IWorkbenchWindow workbenchWindow = workbench.getActiveWorkbenchWindow();

				if (workbenchWindow != null) {
					try {
						instance.snykView = SnykView.getInstance();
					} catch (PartInitException partInitException) {
						logError(partInitException);
					}
				}
			});
		}

		return instance.snykView;
	}

	public static ISnykToolView getView() {
		IWorkbench workbench = PlatformUI.getWorkbench();

		workbench.getDisplay().syncExec(() -> {
			IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

			try {
				snykToolView = (SnykToolView) activePage.showView(SnykToolView.ID);
			} catch (PartInitException e) {
				SnykLogger.logError(e);
			}
		});
		return snykToolView;
	}

	private boolean downloadLS() {
		File lsFile = new File(Preferences.getInstance().getCliPath());
		logger.info("LS: Expecting file at " + lsFile.getAbsolutePath());
		if (!Preferences.getInstance().isManagedBinaries()) {
			logger.info("LS: Managed binaries disabled, skipping download");
			return false;
		}
		if (lsFile.exists()) {
			logger.info("LS: File already exists, checking for age " + lsFile.getAbsolutePath());
			try {
				BasicFileAttributes basicFileAttributes;
				basicFileAttributes = Files.readAttributes(lsFile.toPath(), BasicFileAttributes.class);
				Instant lastModified = basicFileAttributes.lastModifiedTime().toInstant();
				boolean needsUpdate = lastModified.isBefore(Instant.now().minus(4, ChronoUnit.DAYS))
						|| !Preferences.getInstance().getLspVersion().equals(LsBinaries.REQUIRED_LS_PROTOCOL_VERSION);
				logger.info(
						String.format("LS: Needs update? %s. Required LSP version=%s, actual version=%s", needsUpdate,
								LsBinaries.REQUIRED_LS_PROTOCOL_VERSION, Preferences.getInstance().getLspVersion()));
				return needsUpdate;
			} catch (IOException e) {
				SnykLogger.logError(e);
				return false;
			}
		}
		return true;
	}

	static LsDownloader getLsDownloader() throws URISyntaxException {
		return new LsDownloader(HttpClientFactory.getInstance(), runtimeEnvironment, logger);
	}

	public static IStatus download(IProgressMonitor monitor) {
		final File lsFile = new File(Preferences.getInstance().getCliPath());
		try {
			LsDownloader lsDownloader = getLsDownloader();
			lsFile.getParentFile().mkdirs();
			lsDownloader.download(monitor);
			lsFile.setExecutable(true);
		} catch (RuntimeException | URISyntaxException e) {
			return Status.error("Download of Snyk Language Server failed", e);
		}
		return Status.OK_STATUS;
	}

	public static boolean isDownloading() {
		return downloading;
	}

	public void setLogger(ILog logger) {
		SnykStartup.logger = logger;
	}
}
