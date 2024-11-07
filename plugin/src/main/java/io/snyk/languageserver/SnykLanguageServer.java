package io.snyk.languageserver;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Paths;
import java.util.List;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.lsp4e.LanguageServersRegistry;
import org.eclipse.lsp4e.LanguageServersRegistry.LanguageServerDefinition;
import org.eclipse.lsp4e.LanguageServiceAccessor;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import io.snyk.eclipse.plugin.utils.SnykLogger;

@SuppressWarnings("restriction")
public class SnykLanguageServer extends ProcessStreamConnectionProvider implements StreamConnectionProvider {
  public static final String LANGUAGE_SERVER_ID = "io.snyk.languageserver";
  public static final LanguageServerDefinition definition = LanguageServersRegistry.getInstance().getDefinition(SnykLanguageServer.LANGUAGE_SERVER_ID);
  private final LsRuntimeEnvironment runtimeEnvironment;

  public SnykLanguageServer() {
    runtimeEnvironment = new LsRuntimeEnvironment();
  }

  @Override
  public void start() throws IOException {
	Preferences prefs = Preferences.getInstance();
    while (SnykStartup.isDownloading() || !Paths.get(prefs.getCliPath()).toFile().exists()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
    }

	List<String> commands = Lists.of(prefs.getCliPath(), "language-server", "-l", "info");
    String workingDir = SystemUtils.USER_DIR;
    setCommands(commands);
    setWorkingDirectory(workingDir);
    super.start();
  }
  
  public static void startSnykLanguageServer() {
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
    LsConfigurationUpdater.Settings currentSettings = null;
    try {
      currentSettings = new LsConfigurationUpdater().getCurrentSettings();
    } catch (RuntimeException e) {
      SnykLogger.logError(e);
    }
    return currentSettings;
  }
}
