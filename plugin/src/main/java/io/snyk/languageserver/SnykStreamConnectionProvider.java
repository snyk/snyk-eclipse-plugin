package io.snyk.languageserver;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import org.apache.commons.lang3.SystemUtils;
import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class SnykStreamConnectionProvider extends ProcessStreamConnectionProvider implements StreamConnectionProvider {
  public static final String LANGUAGE_SERVER_ID = "io.snyk.languageserver";
  private final LsRuntimeEnvironment runtimeEnvironment = new LsRuntimeEnvironment(new Preferences());

  public SnykStreamConnectionProvider() {
  }

  @Override
  public void start() throws IOException {
    while(SnykStartup.isDownloading()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
    }

    List<String> commands = Lists.of(runtimeEnvironment.getLSFile().getCanonicalPath(), "-l", "debug", "-f",
        runtimeEnvironment.getLSFile().getParent() + File.separator + "snyk-ls.log");
    String workingDir = SystemUtils.USER_DIR;
    setCommands(commands);
    setWorkingDirectory(workingDir);
    super.start();
  }

  @Override
  protected ProcessBuilder createProcessBuilder() {
    var pb = super.createProcessBuilder();
    runtimeEnvironment.updateEnvironment(pb.environment());
    return pb;
  }
}
