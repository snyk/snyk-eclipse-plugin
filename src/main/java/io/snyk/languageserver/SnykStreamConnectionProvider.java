package io.snyk.languageserver;

import org.eclipse.lsp4e.server.ProcessStreamConnectionProvider;
import org.eclipse.lsp4e.server.StreamConnectionProvider;

import java.util.LinkedList;

public class SnykStreamConnectionProvider extends ProcessStreamConnectionProvider implements StreamConnectionProvider {
	private static final String EXECUTABLE_MAC = "snyk-lsp";
	private static final String EXECUTABLE_LINUX = "snyk-lsp-linux";
	private static final String EXECUTABLE_WIN = "snyk-lsp.exe";
	private static final String OS = System.getProperty("os.name").toLowerCase();

	public SnykStreamConnectionProvider() {
		LinkedList<String> commands = new LinkedList<>();
		commands.add(getExecutableNameByOs());
		setCommands(commands);
		String workingDir = System.getenv("HOME");
		setWorkingDirectory(workingDir);
	}

	String getExecutableNameByOs() {
		if (isMac()) return EXECUTABLE_MAC;
		else if (isUnix()) return EXECUTABLE_LINUX;
		else if (isWindows()) return EXECUTABLE_WIN;
		else throw new IllegalStateException("Cannot determine operating system");
	}

	boolean isWindows() {
		return (OS.contains("win"));
	}

	boolean isMac() {
		return (OS.contains("mac"));
	}

	boolean isUnix() {
		return (OS.contains("nix") || OS.contains("nux") || OS.contains("aix"));
	}
}
