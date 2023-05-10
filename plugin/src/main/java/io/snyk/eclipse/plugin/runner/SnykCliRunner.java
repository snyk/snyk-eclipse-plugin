package io.snyk.eclipse.plugin.runner;

import io.snyk.eclipse.plugin.exception.NotSupportedException;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.util.List;
import java.util.Optional;

import static io.snyk.eclipse.plugin.utils.MockHandler.MOCK;
import static io.snyk.eclipse.plugin.utils.MockHandler.getMockScanResult;

public class SnykCliRunner {

  private static final String TEST_PARAMS = "test";
  private static final String FILE_PARAM = "--file=";

  private static final String MONITOR_PARAM = "monitor";

  //	private static final String AUTH_PARAM = "auth";
  private static final String CONFIG_PARAM = "config";
  private static final String IGNORE_PARAM = "ignore";
  private static final String JSON_PARAM = "--json";

  ProcessRunner processRunner = new ProcessRunner();

  public ProcessResult snykConfig() {
    return snykRun(Lists.of(CONFIG_PARAM));
  }

  public ProcessResult snykMonitor(File navigatePath) {
    return snykRun(Lists.of(MONITOR_PARAM, JSON_PARAM), Optional.of(navigatePath));
  }


  public ProcessResult snykTestFile(String rawPath, File navigatePath) {
    return snykRun(Lists.of(TEST_PARAMS, JSON_PARAM, FILE_PARAM + rawPath), Optional.of(navigatePath));
  }

  public ProcessResult snykTest(File navigatePath) {
    if (MOCK) return getMockScanResult();
    return snykRun(Lists.of(TEST_PARAMS, JSON_PARAM), Optional.of(navigatePath));
  }

  public ProcessResult snykIgnore(String id, File navigatePath) {
    String idParam = "--id='" + id + "'";

    return snykRun(Lists.of(IGNORE_PARAM, idParam), Optional.of(navigatePath));
  }

  private ProcessResult snykRun(List<String> arguments) {
    return snykRun(arguments, Optional.empty());
  }


  private ProcessResult snykRun(List<String> arguments, Optional<File> navigatePath) {
    try {
      checkIfTrusted(navigatePath.get());
      SnykExtendedLanguageClient.getInstance().refreshOAuthToken();
      ProcessBuilder processBuilder = createProcessBuilderByOS(arguments, Preferences.getInstance().getPath());
      return processRunner.run(processBuilder, navigatePath);
    } catch (Exception e) {
      return ProcessResult.error(e.getMessage());
    }
  }
  
  private void checkIfTrusted(File file) {
    var trustedPaths = Preferences.getInstance().getPref(Preferences.TRUSTED_FOLDERS, "");
    if (!trustedPaths.contains(file.getAbsolutePath())) {
      throw new UntrustedScanRequestedException(file.getAbsolutePath() + " is not trusted.");
    }
  }

  private ProcessBuilder createProcessBuilderByOS(List<String> params, Optional<String> path) throws Exception {
    ProcessBuilder processbuilder;

    if (SystemUtils.IS_OS_MAC) {
      processbuilder = processRunner.createMacProcessBuilder(params, path);
    } else if (SystemUtils.IS_OS_LINUX) {
      processbuilder = processRunner.createLinuxProcessBuilder(params, path);
    } else if (SystemUtils.IS_OS_WINDOWS) {
      processbuilder = processRunner.createWinProcessBuilder(params, path);
    } else {
      throw new NotSupportedException("This operating system is not supported");
    }

    return processbuilder;
  }
}
