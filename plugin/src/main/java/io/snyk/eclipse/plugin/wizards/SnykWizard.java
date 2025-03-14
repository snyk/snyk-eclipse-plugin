package io.snyk.eclipse.plugin.wizards;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class SnykWizard extends Wizard implements INewWizard {
	protected SnykWizardConfigureAPIPage configureAPIPage;
	protected SnykWizardAuthenticatePage authenticatePage;
	protected IWorkbench workbench;
	protected IStructuredSelection selection;

	public SnykWizard() {
		super();
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
		setNeedsProgressMonitor(true);
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.workbench = workbench;
		this.selection = selection;
	}

	@Override
	public boolean canFinish() {
		if (this.getContainer().getCurrentPage().equals(authenticatePage)) {
			return true;
		}
		return false;
	}

	@Override
	public boolean performCancel() {
		return true;
	}

	@Override
	public boolean performFinish() {
		final var endpoint = authenticatePage.getEndpoint().getText();
		final var unknownCerts = Boolean.toString(authenticatePage.getUnknownCerts().getSelection());

		new Job("Applying configuration from wizard...") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				monitor.beginTask("starting", 60);
				SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
				while (SnykStartup.isDownloading() || lc == null) {
					monitor.subTask("waiting for download");
					try {
						Thread.sleep(1000);
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
						return Status.error(e.getMessage());
					}
					lc = SnykExtendedLanguageClient.getInstance();
				}
				monitor.subTask("saving preferences");
				Preferences prefs = Preferences.getInstance();

				prefs.store(Preferences.ENDPOINT_KEY, endpoint);
				prefs.store(Preferences.INSECURE_KEY, unknownCerts);
				monitor.subTask("updating language server configuration");
				lc.updateConfiguration();
				monitor.worked(20);

				monitor.subTask("ensuring authentication...");
				lc.triggerAuthentication();
				monitor.worked(20);

				monitor.subTask("trusting workspace folders...");
				var projects = ResourceUtils.getAccessibleTopLevelProjects();
				if (projects != null && !projects.isEmpty()) {
					lc.trustWorkspaceFolders();
				}
				monitor.worked(20);
				monitor.done();
				return Status.OK_STATUS;
			}
		}.schedule();
		return true;
	}

	public static void createAndLaunch() {
		SnykWizard wizard = new SnykWizard();
		WizardDialog dialog = new WizardDialog(PlatformUI.getWorkbench().getDisplay().getActiveShell(), wizard);
		dialog.setBlockOnOpen(false);
		dialog.open();
	}
}
