package io.snyk.eclipse.plugin.runner;

import io.snyk.eclipse.plugin.properties.Preferences;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Optional;

public class ProcessRunner {

  private final Preferences preferences;
  private final Bundle bundle;
  private final ILog log;

  private static final String ENV_SNYK_API = "SNYK_API";
  private static final String ENV_SNYK_TOKEN = "SNYK_TOKEN";
  private static final String ENV_SNYK_INTEGRATION_NAME = "SNYK_INTEGRATION_NAME";
  private static final String ENV_SNYK_INTEGRATION_VERSION = "SNYK_INTEGRATION_VERSION";

  private static final String HOME = System.getProperty("user.home");
  private static final String DEFAULT_MAC_PATH = "/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:" + HOME + "/bin:" + HOME + "/.cargo/bin:" + System.getenv("GOPATH") + "/bin" + System.getenv("GOROOT") + "/bin";
  private static final String DEFAULT_LINUX_PATH = DEFAULT_MAC_PATH;
  private static final String DEFAULT_WIN_PATH = "";

  public ProcessRunner(Preferences p) {
    preferences = p;
    this.log = Platform.getLog(ProcessRunner.class);
    bundle = FrameworkUtil.getBundle(ProcessRunner.class);
  }

  public ProcessRunner(Preferences preferences, Bundle bundle, ILog log) {
    this.preferences = preferences;
    this.bundle = bundle;
    this.log = log;
  }

  public ProcessResult run(ProcessBuilder pb, Optional<File> navigatePath) {
    try {
      String line;
      StringBuilder content = new StringBuilder();
      StringBuilder error = new StringBuilder();

      navigatePath.ifPresent(path -> pb.directory(navigatePath.get()));
      Process p = pb.start();

      BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
      BufferedReader stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));

      while ((line = stdInput.readLine()) != null) {
        content.append(line);
      }

      while ((line = stdError.readLine()) != null) {
        error.append(line);
      }

      return new ProcessResult(content.toString(), error.toString());

    } catch (IOException e) {
      e.printStackTrace();
      return new ProcessResult("", e.getMessage());
    }
  }

  public ProcessBuilder createLinuxProcessBuilder(String command, Optional<String> path) {
    return getProcessBuilder(command, path, DEFAULT_LINUX_PATH);
  }

  private ProcessBuilder getProcessBuilder(String command, Optional<String> path, String defaultLinuxPath) {
    ProcessBuilder pb = new ProcessBuilder("sh", "-c", command);
    setupProcessBuilderBase(pb);
    if (path.isPresent()) {
      pb.environment().put("PATH", path.map(p -> p + ":" + defaultLinuxPath).orElse(defaultLinuxPath) + File.pathSeparator + System.getenv("PATH"));
    }
    return pb;
  }

  private void setupProcessBuilderBase(ProcessBuilder pb) {
    String endpoint = preferences.getEndpoint();
    if (endpoint != null && !endpoint.isEmpty()) {
      pb.environment().put(ENV_SNYK_API, endpoint);
    }

    String organization = preferences.getPref(Preferences.ORGANIZATION_KEY);
    if (organization != null) {
      pb.environment().put(Preferences.ORGANIZATION_KEY, organization);
      pb.command().add("--org=" + organization);
    }

    String token = preferences.getAuthToken();
    if (token != null) pb.environment().put(ENV_SNYK_TOKEN, preferences.getAuthToken());

    String insecure = preferences.getPref(Preferences.INSECURE_KEY);
    if (insecure != null) pb.command().add("--insecure");

    String enableTelemetry = preferences.getPref(Preferences.ENABLE_TELEMETRY);
    if (!enableTelemetry.isBlank() && Boolean.parseBoolean(enableTelemetry)) {
      pb.environment().put(Preferences.ENABLE_TELEMETRY, "0");
    } else {
      pb.environment().put(Preferences.ENABLE_TELEMETRY, "1"); // default to disable telemetry
    }

    pb.environment().put(ENV_SNYK_INTEGRATION_NAME, "ECLIPSE");
    pb.environment().put(ENV_SNYK_INTEGRATION_VERSION, getVersion());
  }

  public ProcessBuilder createMacProcessBuilder(String command, Optional<String> path) {
    return getProcessBuilder(command, path, DEFAULT_MAC_PATH);
  }

  public ProcessBuilder createWinProcessBuilder(String command, Optional<String> path) {
    if (command.startsWith("\"/")) command = command.replaceFirst("/", "");
    ProcessBuilder pb = new ProcessBuilder("cmd.exe", "/c ", command);
    setupProcessBuilderBase(pb);
    pb.environment().put("PATH", path.map(p -> p + ";" + DEFAULT_WIN_PATH).orElse(DEFAULT_WIN_PATH) + File.pathSeparator + System.getenv("PATH"));

    // debug logging on windows machines
    IStatus[] statuses = new IStatus[]{new Status(Status.INFO, bundle.getSymbolicName(), "env.PATH = " + pb.environment().get("PATH")), new Status(Status.INFO, bundle.getSymbolicName(), "path = " + path), new Status(Status.INFO, bundle.getSymbolicName(), "command = " + command),};
    MultiStatus multiStatusCommand = new MultiStatus(bundle.getSymbolicName(), Status.INFO, statuses, "Snyk command execution", null);
    log.log(multiStatusCommand);

    return pb;
  }

  public String getVersion() {
    return bundle.getVersion().toString();
  }
}
