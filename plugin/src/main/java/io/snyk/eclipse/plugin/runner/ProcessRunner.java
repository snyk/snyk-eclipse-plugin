package io.snyk.eclipse.plugin.runner;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getCliFile;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import io.snyk.languageserver.LsRuntimeEnvironment;
import io.snyk.languageserver.protocolextension.messageObjects.OAuthToken;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.snyk.eclipse.plugin.EnvironmentConstants;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;

public class ProcessRunner {

  private final Bundle bundle;
  private final ILog log;
  private LsRuntimeEnvironment environment;

  private static final String HOME = System.getProperty("user.home");
  private static final String DEFAULT_MAC_PATH = "/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:" + HOME + "/bin:"
      + HOME + "/.cargo/bin:" + System.getenv("GOPATH") + "/bin" + System.getenv("GOROOT") + "/bin";
  private static final String DEFAULT_LINUX_PATH = DEFAULT_MAC_PATH;
  private static final String DEFAULT_WIN_PATH = "";

  public ProcessRunner() {
    this.log = Platform.getLog(ProcessRunner.class);
    this.bundle = FrameworkUtil.getBundle(ProcessRunner.class);
    this.environment = new LsRuntimeEnvironment();
  }

  public ProcessRunner(Bundle bundle, ILog log, LsRuntimeEnvironment environment) {
    this.bundle = bundle;
    this.log = log;
    this.environment = environment;
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
      SnykLogger.logError(e);
      return new ProcessResult("", e.getMessage());
    }
  }

  public ProcessBuilder createLinuxProcessBuilder(List<String> params, Optional<String> path) {
    return getProcessBuilder(params, path, DEFAULT_LINUX_PATH);
  }

  private ProcessBuilder getProcessBuilder(List<String> params, Optional<String> path, String defaultPathForOS) {
    var cmd = new ArrayList<String>(params.size() + 2);
    cmd.add(getCliFile().getAbsolutePath());
    cmd.addAll(params);

    ProcessBuilder pb = new ProcessBuilder(cmd);
    setupProcessBuilderBase(pb);
    // TODO: move to runtimeEnvironment
    if (path.isPresent() && !path.get().isBlank()) {
      pb.environment().put("PATH",
          path.map(p -> p + File.pathSeparator + defaultPathForOS).orElse(defaultPathForOS)
              + File.pathSeparator + System.getenv("PATH"));
    }
    return pb;
  }

  private void setupProcessBuilderBase(ProcessBuilder pb) {
    environment.addPath(pb.environment());
    environment.addProxyToEnv(pb.environment());
    environment.addAdditionalParamsAndEnv(pb.environment());
    environment.addIntegrationInfoToEnv(pb.environment());

    String endpoint = Preferences.getInstance().getEndpoint();
    if (endpoint != null && !endpoint.isEmpty()) {
      pb.environment().put(EnvironmentConstants.ENV_SNYK_API, endpoint);
    }

    String organization = Preferences.getInstance().getPref(Preferences.ORGANIZATION_KEY);
    if (organization != null && !organization.isBlank()) {
      pb.environment().put(Preferences.ORGANIZATION_KEY, organization);
      pb.command().add("--org=" + organization);
    }

    String additionalParameters = Preferences.getInstance().getPref(Preferences.ADDITIONAL_PARAMETERS);
    if (additionalParameters != null && !additionalParameters.isBlank()) {
      var split = additionalParameters.split(" ");
      for (String param : split) {
        pb.command().add(param);
      }
    }

    String token = Preferences.getInstance().getAuthToken();
    if (token != null && !token.isBlank()) {
      if (Preferences.getInstance().getBooleanPref(Preferences.USE_TOKEN_AUTH, false)) {
    	  pb.environment().put(EnvironmentConstants.ENV_SNYK_TOKEN, token);
    	  pb.environment().remove(EnvironmentConstants.ENV_OAUTH_ACCESS_TOKEN);
      } else {
	      try {
		      ObjectMapper objectMapper = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		      var oauthToken = objectMapper.readValue(token, OAuthToken.class);
		      pb.environment().put(EnvironmentConstants.ENV_OAUTH_ACCESS_TOKEN, oauthToken.getAccessToken());
		      pb.environment().remove(EnvironmentConstants.ENV_SNYK_TOKEN);
	      } catch (Exception e) {
	    	  SnykLogger.logInfo(token);
	      }
      }
    }

    String insecure = Preferences.getInstance().getPref(Preferences.INSECURE_KEY);
    if (insecure != null && insecure.equalsIgnoreCase("true"))
      pb.command().add("--insecure");

  }

  public ProcessBuilder createMacProcessBuilder(List<String> params, Optional<String> path) {
    return getProcessBuilder(params, path, DEFAULT_MAC_PATH);
  }

  public ProcessBuilder createWinProcessBuilder(List<String> params, Optional<String> path) {
    var cmd = new ArrayList<String>(params.size() + 2);
    cmd.add("cmd.exe");
    cmd.add("/c");
    cmd.add(getCliFile().getAbsolutePath());
    cmd.addAll(params);

    ProcessBuilder pb = new ProcessBuilder(cmd);
    setupProcessBuilderBase(pb);
    pb.environment().put("PATH", path.map(p -> p + ";" + DEFAULT_WIN_PATH).orElse(DEFAULT_WIN_PATH)
        + File.pathSeparator + System.getenv("PATH"));

    // debug logging on windows machines
    IStatus[] statuses = new IStatus[] {
        new Status(Status.INFO, bundle.getSymbolicName(), "env.PATH = " + pb.environment().get("PATH")),
        new Status(Status.INFO, bundle.getSymbolicName(), "path = " + path),
        new Status(Status.INFO, bundle.getSymbolicName(), "params = " + params), };
    MultiStatus multiStatusCommand = new MultiStatus(bundle.getSymbolicName(), Status.INFO, statuses,
        "Snyk params execution", null);
    log.log(multiStatusCommand);

    return pb;
  }

  public String getVersion() {
    return bundle.getVersion().toString();
  }
}
