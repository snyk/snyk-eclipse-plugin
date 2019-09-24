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
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.exception.AuthException;
import io.snyk.eclipse.plugin.exception.NotSupportedException;
import io.snyk.eclipse.plugin.properties.Preferences;
import io.snyk.eclipse.plugin.utils.Lists;
import io.snyk.eclipse.plugin.views.SnykView;

public class SnykCliRunner {

	private static final String SNYK_CLI_MAC = "snyk-macos";
	private static final String SNYK_CLI_LINUX = "snyk-linux";
	private static final String SNYK_CLI_WIN = "snyk-win.exe";

	private static final String TEST_PARAMS = "test --json";
	private static final String FILE_PARAM = "--file=";
	
	private static final String MONITOR_PARAM = "monitor --json";

	private static final String AUTH_PARAM = "auth";
	private static final String CONFIG_PARAM = "config";
	private static final String IGNORE_PARAM = "ignore";
	
	private static final String NO_AUTH_TOKEN = "Snyk isn’t yet configured, please authenticate in preferences page";
	

	ProcessRunner processRunner = new ProcessRunner();
	
	public ProcessResult snykAuth() {
		String authToken = Preferences.getAuthToken();
		if (authToken == null || authToken.isEmpty()) {
			try {
				String apiToken = Authenticator.INSTANCE.callLogin();
				Preferences.store(Preferences.AUTH_TOKEN_KEY, apiToken);
				authToken = Preferences.getAuthToken();
			} catch (AuthException e) {
				e.printStackTrace();
				return ProcessResult.error(e.getMessage());
				
			}
		}
		return snykRun(Lists.of​(AUTH_PARAM, authToken));
	}
	
	public ProcessResult snykConfig() {
		return snykRun(Lists.of​(CONFIG_PARAM));
	}
	
	public ProcessResult snykSetEndpoint(String url) {
		return snykRun(Lists.of​(CONFIG_PARAM, "set endpoint=" + url));
	}	
	
	public ProcessResult snykMonitor(File navigatePath) {
		return snykRun(Lists.of​(MONITOR_PARAM), Optional.of(navigatePath));
	}


	public ProcessResult snykTestFile(String rawPath, File navigatePath) {
		return snykRun(Lists.of​(TEST_PARAMS, FILE_PARAM+rawPath), Optional.of(navigatePath));
	}

	public ProcessResult snykTest(File navigatePath) {
		if (MOCK) return getMockScanResult();
		return snykRun(Lists.of​(TEST_PARAMS), Optional.of(navigatePath));
	}
	
	public ProcessResult snykIgnore(String id, File navigatePath) {
		String idParam = "--id='" + id + "'";
		
		return snykRun(Lists.of​(IGNORE_PARAM, idParam), Optional.of(navigatePath));
	}
	
	private ProcessResult snykRun(List<String> arguments) {
		return snykRun(arguments, Optional.empty());
	}
	
	
	private ProcessResult snykRun(List<String> arguments, Optional<File> navigatePath) {
		try {
			ProcessBuilder processBuilder = createProcessBuilderByOS(arguments, Preferences.getPath());
			return processRunner.run(processBuilder, navigatePath);
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
