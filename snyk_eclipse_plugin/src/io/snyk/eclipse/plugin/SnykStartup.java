package io.snyk.eclipse.plugin;

import java.io.File;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IStartup;

import io.snyk.eclipse.plugin.utils.CliDownloader;
import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getCliFile;

public class SnykStartup implements IStartup {

	@Override
	public void earlyStartup() {
		Job job = new Job("Downloading latest Snyk CLI release...") {		
			
			@Override
		    protected IStatus run(IProgressMonitor monitor) {					
				try {					
					File cliFile = getCliFile();
					
					if (!cliFile.exists()) {	
						cliFile.getParentFile().mkdirs();
						
						CliDownloader.newInstance().download(cliFile, monitor);	
						
						cliFile.setExecutable(true);										
					}
				} catch (Exception exception) {
					exception.printStackTrace();
				}																				       
		        
		        return Status.OK_STATUS;
		    }

		};
		
		job.setPriority(Job.LONG);
		
		job.schedule();
	}	
}
