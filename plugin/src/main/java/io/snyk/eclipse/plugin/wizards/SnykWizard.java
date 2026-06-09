package io.snyk.eclipse.plugin.wizards;

import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.TrustedFoldersHelper;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class SnykWizard extends Wizard implements INewWizard {
	protected SnykWizardAuthenticatePage authenticatePage;
	protected IWorkbench workbench;
	protected IStructuredSelection selection;

	private WizardDialog wizardDialog;

	public SnykWizard() {
		super();
	}

	@Override
	public String getWindowTitle() {
		return "Snyk Wizard";
	}

	@Override
	public void addPages() {
		authenticatePage = new SnykWizardAuthenticatePage();
		addPage(authenticatePage);
		setNeedsProgressMonitor(false);
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.workbench = workbench;
		this.selection = selection;
	}

	@Override
	public boolean canFinish() {
		return true;
	}

	@Override
	public boolean performCancel() {
		return true;
	}

	@Override
	public boolean performFinish() {
		// Disable Finish immediately to prevent double-click spawning two auth Jobs.
		if (getContainer().getCurrentPage() != null) {
			getContainer().updateButtons();
		}
		authenticatePage.setPageComplete(false);

		new Job("Authenticating with Snyk...") {
			@Override
			@SuppressWarnings("PMD.AvoidCatchingGenericException")
			protected IStatus run(IProgressMonitor monitor) {
				monitor.beginTask("Starting", 3);

				monitor.subTask("Waiting for language server...");
				SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
				while (SnykStartup.isDownloading() || lc == null) {
					try {
						Thread.sleep(1000);
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
						return Status.error(e.getMessage());
					}
					lc = SnykExtendedLanguageClient.getInstance();
				}
				monitor.worked(1);

				monitor.subTask("Logging out...");
				lc.logout();
				monitor.worked(1);

				monitor.subTask("Opening browser for authentication...");
				final SnykExtendedLanguageClient finalLc = lc;
				var loginFuture = lc.triggerAuthentication();

				AuthWaitDialog[] dialogHolder = new AuthWaitDialog[1];
				Display.getDefault().syncExec(() -> {
					Shell shell = wizardDialog != null ? wizardDialog.getShell() : null;
					if (shell == null || shell.isDisposed()) {
						return;
					}
					AuthWaitDialog dialog = new AuthWaitDialog(shell);
					dialog.setOnCancel(finalLc::cancelLogin);
					dialog.setBlockOnOpen(false);
					dialog.open();
					dialogHolder[0] = dialog;
				});

				AuthWaitDialog waitDialog = dialogHolder[0];

				if (waitDialog != null) {
					waitDialog.setCopyUrlEnabled(true);
				}

				monitor.worked(1);
				monitor.done();

				boolean success = false;
				try {
					loginFuture.get();
					success = true;
				} catch (Exception e) { // NOPMD
					// cancelled or failed — close below
				}

				if (success) {
					trustWorkspaceFoldersClientSide();
					finalLc.updateConfiguration();
				}

				if (waitDialog != null) {
					waitDialog.closeDialog();
				}

				closeWizard();

				return Status.OK_STATUS;
			}
		}.schedule();

		// Return false so JFace does not auto-close the wizard.
		return false;
	}

	private void trustWorkspaceFoldersClientSide() {
		List<IProject> projects = ResourceUtils.getAccessibleTopLevelProjects();
		if (projects == null || projects.isEmpty()) {
			return;
		}
		String[] paths = projects.stream()
				.filter(p -> p.getLocation() != null)
				.map(p -> p.getLocation().toFile().getAbsolutePath())
				.collect(Collectors.toList())
				.toArray(new String[0]);
		TrustedFoldersHelper.addTrustedFolders(paths);
	}

	private void closeWizard() {
		Display.getDefault().asyncExec(() -> {
			if (wizardDialog != null) {
				Shell shell = wizardDialog.getShell();
				if (shell != null && !shell.isDisposed()) {
					wizardDialog.close();
				}
			}
		});
	}

	public static void createAndLaunch() {
		Display.getDefault().asyncExec(() -> {
			SnykWizard wizard = new SnykWizard();
			Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
			WizardDialog dialog = new WizardDialog(shell, wizard);
			wizard.wizardDialog = dialog;
			dialog.setBlockOnOpen(false);
			dialog.open();
		});
	}
}
