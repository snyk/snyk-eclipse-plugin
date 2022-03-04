package io.snyk.languageserver;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

import org.apache.commons.lang3.SystemUtils;
import org.apache.http.impl.client.HttpClients;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;

import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import io.snyk.languageserver.download.LsDownloader;

public class SnykStreamConnectionProvider extends ProcessStreamConnectionProvider
		implements StreamConnectionProvider, IJobChangeListener {
	private final LsUtils utils = new LsUtils(new Preferences());
	private static ReentrantLock scheduled = new ReentrantLock();
	private static boolean downloadStarted = false;

	public SnykStreamConnectionProvider() {
		if (!downloadStarted) {
			downloadStarted = true;
			Job downloadLSP = getJob();
			downloadLSP.setPriority(Job.LONG);
			downloadLSP.addJobChangeListener(this);
			try {
				scheduled.lock();
				downloadLSP.schedule();
			} finally {
				scheduled.unlock();
			}
		}
	}

	@Override
	public void start() throws IOException {
		try {
			scheduled.lock();
			List<String> commands = Lists.of(utils.getLSFile().getCanonicalPath());
			String workingDir = SystemUtils.USER_DIR;
			setCommands(commands);
			setWorkingDirectory(workingDir);
			super.start();
		} finally {
			scheduled.unlock();
		}
	}

	private Job getJob() {
		return new Job("Downloading latest Snyk LSP ...") {
			private final File lsFile = utils.getLSFile();

			@SuppressWarnings("ResultOfMethodCallIgnored")
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					if (!update())
						return Status.OK_STATUS;
					var httpClient = HttpClients.createDefault();
					var lsDownloader = new LsDownloader(utils, httpClient);
					lsFile.getParentFile().mkdirs();
					lsDownloader.download(monitor);
					lsFile.setExecutable(true);
				} catch (RuntimeException | IOException e) {
					return Status.error("Download of Snyk Language Server failed", e);
				}
				return Status.OK_STATUS;
			}

			private boolean update() throws IOException {
				if (lsFile.exists()) {
					BasicFileAttributes basicFileAttributes = Files.readAttributes(lsFile.toPath(),
							BasicFileAttributes.class);
					Instant lastModified = basicFileAttributes.lastModifiedTime().toInstant();
					return lastModified.isBefore(Instant.now().minus(2, ChronoUnit.DAYS));
				}
				return true;
			}
		};
	}

	@Override
	public void done(IJobChangeEvent iJobChangeEvent) {
		if (scheduled.isHeldByCurrentThread())
			scheduled.unlock();
	}

	@Override
	public void aboutToRun(IJobChangeEvent iJobChangeEvent) {

	}

	@Override
	public void awake(IJobChangeEvent iJobChangeEvent) {
	}

	@Override
	public void running(IJobChangeEvent iJobChangeEvent) {

	}

	@Override
	public void scheduled(IJobChangeEvent iJobChangeEvent) {
		scheduled.lock();
	}

	@Override
	public void sleeping(IJobChangeEvent iJobChangeEvent) {
	}
}
