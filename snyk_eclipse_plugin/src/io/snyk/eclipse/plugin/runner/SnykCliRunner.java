package io.snyk.eclipse.plugin.runner;

import static io.snyk.eclipse.plugin.utils.MockHandler.MOCK;
import static io.snyk.eclipse.plugin.utils.MockHandler.getMockScanResult;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.Bundle;

import io.snyk.eclipse.plugin.exception.NotSupportedException;
import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;

public class SnykCliRunner {

//	public static SnykCliRunner INSTANCE = new SnykCliRunner();

	private static final String SNYK_CLI_MAC = "snyk-macos";
	private static final String SNYK_CLI_LINUX = "snyk-linux";
	private static final String SNYK_CLI_WIN = "snyk-win.exe";

	private static final String TEST_PARAMS = "test --json";
	private static final String FILE_PARAM = "--file=";
	
	private static final String MONITOR_PARAM = "monitor --json";

	private static final String AUTH_PARAM = "auth";
	private static final String CONFIG_PARAM = "config";
	
	private static final String NO_AUTH_TOKEN = "Snyk isn’t yet configured, please insert Auth token in preferences page";

	ProcessRunner processRunner = new ProcessRunner();

	
	public ProcessResult snykAuth() {
		String authToken = Preferences.getAuthToken();
		if (authToken == null || authToken.isEmpty()) {
			return ProcessResult.error(NO_AUTH_TOKEN);
		}
		
		try {
			ProcessBuilder processBuilder = createProcessBuilderByOS(Lists.of​(AUTH_PARAM, authToken),Preferences.getPath());
			return processRunner.run(processBuilder, Optional.empty());
		} catch (Exception e) {
			return ProcessResult.error(e.getMessage());
		}
	}
	
	public ProcessResult snykConfig() {
		try {
			ProcessBuilder processBuilder = createProcessBuilderByOS(Lists.of​(CONFIG_PARAM),Preferences.getPath());
			return processRunner.run(processBuilder, Optional.empty());
		} catch (Exception e) {
			return ProcessResult.error(e.getMessage());
		}
	}
	
	public ProcessResult snykMonitor(File navigatePath) {
		try {
			ProcessBuilder processBuilder = createProcessBuilderByOS(Lists.of​(MONITOR_PARAM), Preferences.getPath());
			return processRunner.run(processBuilder, Optional.of(navigatePath));
		} catch (Exception e) {
			return ProcessResult.error(e.getMessage());
		}
	}


	public ProcessResult snykTestFile(String rawPath) {
		try {
			ProcessBuilder processBuilder = createProcessBuilderByOS(Lists.of​(TEST_PARAMS, FILE_PARAM+rawPath),
					Preferences.getPath());
			return processRunner.run(processBuilder, Optional.empty());
		} catch (Exception e) {
			return ProcessResult.error(e.getMessage());
		}
	}

	public ProcessResult snykTest(File navigatePath) {
		if (MOCK) return getMockScanResult();
		try {
			ProcessBuilder processBuilder = createProcessBuilderByOS(Lists.of​(TEST_PARAMS), Preferences.getPath());
			return processRunner.run(processBuilder, Optional.of(navigatePath));
		} catch (Exception e) {
			return ProcessResult.error(e.getMessage());
		}
	}

	private ProcessBuilder createProcessBuilderByOS(List<String> params, Optional<String> path) throws IOException, NotSupportedException {
		String runnable;
		ProcessBuilder processbuilder;
		String paramsString = params.stream().collect(Collectors.joining(" "));
		
		if (SystemUtils.IS_OS_MAC) {
			runnable = getRunnableLocation(SNYK_CLI_MAC);
			processbuilder= processRunner.createMacProcessBuilder(runnable + " " + paramsString, path);
		} else if (SystemUtils.IS_OS_LINUX) {
			runnable = getRunnableLocation(SNYK_CLI_LINUX);
			processbuilder= processRunner.createLinuxProcessBuilder(runnable + " " + paramsString, path);
		} else if (SystemUtils.IS_OS_WINDOWS) {
			runnable = getRunnableLocation(SNYK_CLI_WIN);
			processbuilder= processRunner.createWinProcessBuilder(runnable + " " + paramsString, path);
		} else {
			throw new NotSupportedException("This operating system is not supported");
		}
		
		return processbuilder;
	}

	private String getRunnableLocation(String relativeLoc) throws IOException {
		URL url = new URL(Platform.getInstallLocation().getURL(), relativeLoc);
		return url.getFile();
	}

}
