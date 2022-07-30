package io.snyk.languageserver;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4e.LanguageServerWrapper;
import org.eclipse.lsp4e.LanguageServersRegistry;
import org.eclipse.lsp4e.LanguageServiceAccessor;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.ui.PlatformUI;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class SnykLanguageServer extends ProcessStreamConnectionProvider implements StreamConnectionProvider {
  public static final String LANGUAGE_SERVER_ID = "io.snyk.languageserver";
  private final LsRuntimeEnvironment runtimeEnvironment;

  public SnykLanguageServer() {
    runtimeEnvironment = new LsRuntimeEnvironment();
  }

  @Override
  public void start() throws IOException {
    while (SnykStartup.isDownloading()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
    }

    List<String> commands = Lists.of(Preferences.getInstance().getLsBinary(), "-l", "info", "-f",
        new File(Preferences.getInstance().getLsBinary()).getParent() + File.separator + "snyk-ls.log");
    String workingDir = SystemUtils.USER_DIR;
    setCommands(commands);
    setWorkingDirectory(workingDir);
    super.start();
    new Job("Snyk: Sending preferences to Language Server") {
      @Override
      protected IStatus run(IProgressMonitor monitor) {
        try {
          new LsConfigurationUpdater().configurationChanged();
          monitor.done();
        } catch (RuntimeException e) {
          SnykLogger.logError(e);
        }
        return Status.OK_STATUS;
      }
    }.schedule();
  }

  @Override
  protected ProcessBuilder createProcessBuilder() {
    var pb = super.createProcessBuilder();
    runtimeEnvironment.updateEnvironment(pb.environment());
    return pb;
  }

  @Override
  public Object getInitializationOptions(URI rootUri) {
    LsConfigurationUpdater.Settings currentSettings = null;
    try {
      currentSettings = new LsConfigurationUpdater().getCurrentSettings();
    } catch (RuntimeException e) {
      SnykLogger.logError(e);
    }
    return currentSettings;
  }

  public static void InitializeServer() {
    var projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
    for (IProject project : projects) {
      if (project.isAccessible()) {
        try {
          LanguageServiceAccessor.getLSWrapper(project,
              LanguageServersRegistry.getInstance().getDefinition(SnykLanguageServer.LANGUAGE_SERVER_ID));
        } catch (IOException e) {
          SnykLogger.logError(e);
          return;
        }
      }
    }
  }
}
