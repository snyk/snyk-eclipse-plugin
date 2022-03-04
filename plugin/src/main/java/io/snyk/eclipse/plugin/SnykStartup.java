package io.snyk.eclipse.plugin;

import io.snyk.eclipse.plugin.utils.CliDownloader;
import io.snyk.eclipse.plugin.views.SnykView;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.*;

import java.io.File;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getCliFile;
import static io.snyk.eclipse.plugin.utils.SnykLogger.logError;

public class SnykStartup implements IStartup {

    private SnykView snykView = null;

    @Override
    public void earlyStartup() {
        Job downloadCLIJob = new Job("Downloading latest Snyk CLI release...") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    File cliFile = getCliFile();

                    if (!cliFile.exists()) {
                        SnykView snykView = getSnykView();
                        snykView.disableActions();
                        cliFile.getParentFile().mkdirs();
                        CliDownloader.newInstance().download(cliFile, monitor);
                        cliFile.setExecutable(true);
                        snykView.setRunActionEnabled();
                    }
                } catch (Exception exception) {
                    logError(exception);
                }

                return Status.OK_STATUS;
            }
        };
        downloadCLIJob.setPriority(Job.LONG);
        downloadCLIJob.schedule();
    }

    private SnykView getSnykView() {
        if (snykView == null) {
            IWorkbench workbench = PlatformUI.getWorkbench();

            workbench.getDisplay().syncExec(() -> {
                IWorkbenchWindow workbenchWindow = workbench.getActiveWorkbenchWindow();

                if (workbenchWindow != null) {
                    try {
                        snykView = (SnykView) workbenchWindow.getActivePage().showView(SnykView.ID);
                    } catch (PartInitException partInitException) {
                        logError(partInitException);
                    }
                }
            });
        }

        return snykView;
    }


}