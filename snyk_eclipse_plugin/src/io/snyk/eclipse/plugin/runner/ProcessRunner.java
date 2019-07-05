package io.snyk.eclipse.plugin.runner;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Optional;

public class ProcessRunner {
	
	private static final String DEFAULT_MAC_PATH = "/usr/local/bin";
	private static final String DEFAULT_LINUX_PATH = "/usr/bin";
	private static final String DEFAULT_WIN_PATH = "";
	
		
	public ProcessResult run(ProcessBuilder pb, Optional<File> navigatePath) {
		try {
			String line;
			StringBuilder content  = new StringBuilder();
			StringBuilder error  = new StringBuilder();

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
	
	//TODO implement default install path for linux
	public ProcessBuilder createLinuxProcessBuilder(String command, Optional<String> path) {
		ProcessBuilder pb = new ProcessBuilder("sh", "-c", command);
		pb.environment().put("PATH", path.map(p -> p+":"+DEFAULT_LINUX_PATH).orElse(DEFAULT_LINUX_PATH) + File.pathSeparator + System.getenv("PATH"));
		return pb;
	}
	
	public ProcessBuilder createMacProcessBuilder(String command, Optional<String> path) {
		ProcessBuilder pb = new ProcessBuilder("sh", "-c", command);
		pb.environment().put("PATH", path.map(p -> p+":"+DEFAULT_MAC_PATH).orElse(DEFAULT_MAC_PATH) + File.pathSeparator + System.getenv("PATH"));
		return pb;
	}
	
	//TODO implement for Win
	public ProcessBuilder createWinProcessBuilder(String command, Optional<String> path) {
		ProcessBuilder pb = new ProcessBuilder("cmd.exe", "/c", command);
		pb.environment().put("PATH", path.map(p -> p+":"+DEFAULT_WIN_PATH).orElse(DEFAULT_WIN_PATH) + File.pathSeparator + System.getenv("PATH"));
		return pb;
	}


}
