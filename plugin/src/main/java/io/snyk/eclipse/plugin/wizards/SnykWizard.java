package io.snyk.eclipse.plugin.wizards;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class SnykWizard extends Wizard implements INewWizard {
	protected SnykWizardConfigureAPIPage configureAPIPage;
	protected SnykWizardAuthenticatePage authenticatePage;

	protected SnykWizardModel model;

	protected IWorkbench workbench;
	protected IStructuredSelection selection;

	public SnykWizard() {
		super();
		model = new SnykWizardModel();
		setNeedsProgressMonitor(true);
	}

	@Override
	public String getWindowTitle() {
		return "Snyk Wizard";
	}

	@Override
	public void addPages() {
		configureAPIPage = new SnykWizardConfigureAPIPage();
		addPage(configureAPIPage);

		authenticatePage = new SnykWizardAuthenticatePage();
		addPage(authenticatePage);
	}

	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.workbench = workbench;
		this.selection = selection;
	}

	public boolean canFinish() {
		if (this.getContainer().getCurrentPage() == authenticatePage) {
			return true;
		}
		return false;
	}

	public boolean performCancel() {
		model.resetPreferences();
		return true;
	}

	public boolean performFinish() {
		new Job("Applying configuration from wizard...") {	
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				monitor.beginTask("starting", 60);
				while (SnykStartup.isDownloading()) {
					monitor.subTask("waiting for download");
					try {
						Thread.sleep(1000);
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
						return Status.error(e.getMessage());
					}
				}
				monitor.subTask("updating language server configuration");
				SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
				lc.updateConfiguration();
				monitor.worked(20);
				monitor.subTask("ensuring authentication...");
				lc.triggerAuthentication();
				monitor.worked(20);
				monitor.subTask("trusting workspace folders...");
				var projects = ResourceUtils.getAccessibleTopLevelProjects();
				if (projects != null && projects.size()>0) {
					lc.trustWorkspaceFolders();
				}
				monitor.worked(20);
				monitor.done();
				return Status.OK_STATUS;
			}
		}.schedule();
		return true;
	}
	
	public static Object createAndLaunch() {
		SnykWizard wizard = new SnykWizard();
		WizardDialog dialog = new WizardDialog(Display.getDefault().getActiveShell(),wizard);
		dialog.setBlockOnOpen(true);
		dialog.open();
		return null;
	}
}
