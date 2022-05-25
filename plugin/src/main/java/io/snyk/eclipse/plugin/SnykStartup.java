package io.snyk.eclipse.plugin;

import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.CliDownloader;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.SnykView;
import io.snyk.languageserver.LsRuntimeEnvironment;
import io.snyk.languageserver.download.LsDownloadRequest;
import io.snyk.languageserver.download.LsDownloader;
import org.apache.http.impl.client.HttpClients;
import org.eclipse.core.net.proxy.IProxyData;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.time.temporal.ChronoUnit;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getCliFile;
import static io.snyk.eclipse.plugin.utils.SnykLogger.logError;

public class SnykStartup implements IStartup {
  private LsRuntimeEnvironment runtimeEnvironment;
  private SnykView snykView = null;
  private static boolean downloading = true;
  private ILog logger;

  @Override
  public void earlyStartup() {
    if (logger == null) {
       logger = Platform.getLog(getClass());
    }
    runtimeEnvironment = new LsRuntimeEnvironment(new Preferences());
    Job downloadCLIJob = new Job("Downloading latest Snyk CLI release...") {
      @Override
      protected IStatus run(IProgressMonitor monitor) {
        try {
          File cliFile = getCliFile();
          if (!cliFile.exists()) {
            SnykView snykView = getSnykView();
            snykView.disableRunAbortActions();
            cliFile.getParentFile().mkdirs();
            CliDownloader.newInstance().download(cliFile, monitor);
            cliFile.setExecutable(true);
            snykView.toggleRunActionEnablement();
          }

          logger.info("LS: Checking for needed download");
          if (downloadLS()) {
            logger.info("LS: Need to download");
            downloading = true;
            monitor.subTask("Starting download of Snyk Language Server");
            download(monitor);
          }
        } catch (Exception exception) {
          logError(exception);
        }
        downloading = false;
        return Status.OK_STATUS;
      }
    };
    downloadCLIJob.setPriority(Job.LONG);
    downloadCLIJob.schedule();
  }

  private SnykView getSnykView() {
    if (snykView == null) {
      IWorkbench workbench = PlatformUI.getWorkbench();

      workbench.getDisplay().syncExec(() -> {
        IWorkbenchWindow workbenchWindow = workbench.getActiveWorkbenchWindow();

        if (workbenchWindow != null) {
          try {
            snykView = (SnykView) workbenchWindow.getActivePage().showView(SnykView.ID);
          } catch (PartInitException partInitException) {
            logError(partInitException);
          }
        }
      });
    }

    return snykView;
  }

  private boolean downloadLS() {
    File lsFile = runtimeEnvironment.getLSFile();
    logger.info("LS: Expecting file at " + lsFile.getAbsolutePath());
    String customPath = new Preferences().getLsBinary();
    if (!isDownloadAllowed(customPath)) return false;
    if (lsFile.exists()) {
      logger.info("LS: File already exists, checking for age " + lsFile.getAbsolutePath());
      try {
        BasicFileAttributes basicFileAttributes;
        basicFileAttributes = Files.readAttributes(lsFile.toPath(), BasicFileAttributes.class);
        Instant lastModified = basicFileAttributes.lastModifiedTime().toInstant();
        boolean needsUpdate = lastModified.isBefore(Instant.now().minus(4, ChronoUnit.DAYS));
        logger.info("LS: Needs update? " + needsUpdate);
        return needsUpdate;
      } catch (IOException e) {
        SnykLogger.logError(e);
        return false;
      }
    }
    return true;
  }

  boolean isDownloadAllowed(String customPath) {
    boolean customPathSet = customPath != null && !customPath.isBlank();
    if (customPathSet) {
      logger.info("LS: Custom LS path is set, not downloading. Path: " + customPath);
      return false;
    }
    return true;
  }

  LsDownloader getLsDownloader() throws URISyntaxException {
    URI baseUri = URI.create(LsDownloadRequest.getBaseURL());
    IProxyData[] proxyData = runtimeEnvironment.getProxyService().select(baseUri);
    var relevantProxyData = getRelevantProxyData(proxyData);
    var builder = HttpClients.custom();
    return new LsDownloader(runtimeEnvironment, builder, relevantProxyData, logger);
  }

  private IProxyData getRelevantProxyData(IProxyData[] proxyData) {
    for (IProxyData data : proxyData) {
      if (data.getHost() == null)
        continue;
      return data;
    }
    return null;
  }

  @SuppressWarnings("ResultOfMethodCallIgnored")
  IStatus download(IProgressMonitor monitor) {
    final File lsFile = runtimeEnvironment.getLSFile();
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
    this.logger = logger;
  }
}
