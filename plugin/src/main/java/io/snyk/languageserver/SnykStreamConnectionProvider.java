package io.snyk.languageserver;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.List;

public class SnykStreamConnectionProvider extends ProcessStreamConnectionProvider implements StreamConnectionProvider {
  public static final String LANGUAGE_SERVER_ID = "io.snyk.languageserver";
  private final LsRuntimeEnvironment runtimeEnvironment = new LsRuntimeEnvironment(new Preferences());

  public SnykStreamConnectionProvider() {
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

    List<String> commands = Lists.of(runtimeEnvironment.getLSFile().getCanonicalPath(), "-l", "info", "-f",
      runtimeEnvironment.getLSFile().getParent() + File.separator + "snyk-ls.log");
    String workingDir = SystemUtils.USER_DIR;
    setCommands(commands);
    setWorkingDirectory(workingDir);
    super.start();
    new Job("Snyk: Sending preferences to Language Server") {
      @Override
      protected IStatus run(IProgressMonitor monitor) {
        try {
          new LsConfigurationUpdater().configurationChanged(runtimeEnvironment.getPreferences());
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
	    Preferences preferences = runtimeEnvironment.getPreferences();
		LsConfigurationUpdater lsConfigurationUpdater = new LsConfigurationUpdater();
		currentSettings = lsConfigurationUpdater.getCurrentSettings(preferences);
	} catch (RuntimeException e) {
         SnykLogger.logError(e);
    }
	return currentSettings;
  }
}
