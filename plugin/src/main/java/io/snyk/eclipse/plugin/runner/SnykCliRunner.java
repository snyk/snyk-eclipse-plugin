package io.snyk.eclipse.plugin.runner;

import io.snyk.eclipse.plugin.exception.NotSupportedException;
import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import org.apache.commons.lang3.SystemUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getCliFile;
import static io.snyk.eclipse.plugin.utils.MockHandler.MOCK;
import static io.snyk.eclipse.plugin.utils.MockHandler.getMockScanResult;

public class SnykCliRunner {

    private static final Preferences PREFERENCES = new Preferences();
    private static final String TEST_PARAMS = "test";
    private static final String FILE_PARAM = "--file=";

    private static final String INSECURE = "--insecure";

    private static final String MONITOR_PARAM = "monitor";

    //	private static final String AUTH_PARAM = "auth";
    private static final String CONFIG_PARAM = "config";
    private static final String IGNORE_PARAM = "ignore";
    private static final String JSON_PARAM = "--json";

    ProcessRunner processRunner = new ProcessRunner();

//	public ProcessResult snykAuth() {
//		String authToken = Preferences.getAuthToken();
//		if (authToken == null || authToken.isEmpty()) {
//			try {
//				String apiToken = Authenticator.INSTANCE.callLogin();
//				Preferences.store(Preferences.AUTH_TOKEN_KEY, apiToken);
//				authToken = Preferences.getAuthToken();
//			} catch (AuthException e) {
//				e.printStackTrace();
//				return ProcessResult.error(e.getMessage());
//
//			}
//		}
//		return snykRun(Lists.of​(AUTH_PARAM, authToken));
//	}

    public ProcessResult snykConfig() {
        return snykRun(Lists.of(CONFIG_PARAM));
    }

    public ProcessResult snykSetEndpoint(String url) {
        return snykRun(Lists.of(CONFIG_PARAM, "set endpoint=" + url));
    }

    public ProcessResult snykUnsetEndpoint() {
        return snykRun(Lists.of(CONFIG_PARAM, "unset endpoint"));
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
            if (PREFERENCES.isInsecure()) {
                arguments = new ArrayList<>(arguments);
                arguments.add(INSECURE);
            }

            ProcessBuilder processBuilder = createProcessBuilderByOS(arguments, PREFERENCES.getPath());
            return processRunner.run(processBuilder, navigatePath);
        } catch (Exception e) {
            return ProcessResult.error(e.getMessage());
        }
    }

    private ProcessBuilder createProcessBuilderByOS(List<String> params, Optional<String> path) throws Exception {
        String runnable = getCliFile().getAbsolutePath();
        ProcessBuilder processbuilder;
        String paramsString = params.stream().collect(Collectors.joining("\" \"", "\"", "\""));

        if (SystemUtils.IS_OS_MAC) {
            processbuilder = processRunner.createMacProcessBuilder("\"" + runnable + "\" " + paramsString, path);
        } else if (SystemUtils.IS_OS_LINUX) {
            processbuilder = processRunner.createLinuxProcessBuilder(runnable + " " + paramsString, path);
        } else if (SystemUtils.IS_OS_WINDOWS) {
            processbuilder = processRunner.createWinProcessBuilder("\"" + runnable + "\" " + paramsString, path);
        } else {
            throw new NotSupportedException("This operating system is not supported");
        }

        return processbuilder;
    }
}
