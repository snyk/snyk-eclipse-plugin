package io.snyk.eclipse.plugin;

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
import org.eclipse.swt.SWTException;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView;
import io.snyk.eclipse.plugin.wizards.SnykWizard;
import io.snyk.languageserver.LsRuntimeEnvironment;
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.download.ChecksumVerificationException;
import io.snyk.languageserver.download.HttpClientFactory;
import io.snyk.languageserver.download.LsBinaries;
import io.snyk.languageserver.download.LsDownloader;

public class SnykStartup implements IStartup {
	private static LsRuntimeEnvironment runtimeEnvironment;
	private static SnykToolView snykToolView;
	private static boolean downloading = true;
	private static ILog logger;

	@Override
	public void earlyStartup() {
		if (logger == null) {
			logger = Platform.getLog(getClass());
		}
		runtimeEnvironment = new LsRuntimeEnvironment();
		Job initJob = new Job("Initializing Snyk...") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				monitor.beginTask("Initializing...", 100);
				downloadIfNeeded(monitor);

				// Even if download failed, try to start LS - there might be an existing binary
				monitor.subTask("Starting Language Server...");

				try {
					SnykLanguageServer.startSnykLanguageServer();

					PlatformUI.getWorkbench().getDisplay().syncExec(() -> {
						Preferences prefs = Preferences.getInstance();
						if (!prefs.isAuthenticated() && !prefs.isTest()) {
							monitor.subTask("Starting Snyk Wizard to configure initial settings...");
							SnykWizard.createAndLaunch();
						}
					});
				} catch (RuntimeException e) { // NOPMD - intentional catch-all of runtime exceptions for UI initialization
					SnykLogger.logError(e);
				}
				monitor.done();
				return Status.OK_STATUS;
			}

			private void downloadIfNeeded(IProgressMonitor monitor) {
				try {
					logger.info("LS: Checking for needed download");
					if (downloadLS()) {
						monitor.subTask("Downloading CLI");
						logger.info("LS: Need to download");
						downloading = true;
						IStatus status = download(monitor);
						if (!status.isOK()) {
							logDownloadFailure(status);
						}
					}
				} catch (Exception e) { // NOPMD - intentional catch-all for download failures
					logDownloadFailure(e.getMessage());
				} finally {
					downloading = false;
				}
			}

			private void logDownloadFailure(IStatus status) {
				String errorMessage = status.getMessage();
				if (status.getException() != null) {
					errorMessage += ": " + status.getException().getMessage();
				}
				logDownloadFailure(errorMessage);
			}

			private void logDownloadFailure(String errorMessage) {
				// Log the error - user may just be offline but have an existing binary. We show a user-facing error later if binary is missing.
				String message = "Failed to download Snyk CLI: " + errorMessage
						+ ". Will try to start with existing binary if available.";
				logger.error(message);
			}
		};
		initJob.setPriority(Job.LONG);
		initJob.schedule();
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
						|| !LsBinaries.REQUIRED_LS_PROTOCOL_VERSION.equals(Preferences.getInstance().getLspVersion());
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
		} catch (RuntimeException | IOException | URISyntaxException | ChecksumVerificationException e) { // NOPMD - intentional catch-all of runtime exceptions for download errors
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
