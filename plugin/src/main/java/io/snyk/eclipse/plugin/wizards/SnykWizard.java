package io.snyk.eclipse.plugin.wizards;

import java.io.File;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
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
	private static final ILog LOG = Platform.getLog(SnykWizard.class);
	// Class-level guard: prevents concurrent auth across multiple SnykWizard instances.
	private static final AtomicBoolean IN_FLIGHT = new AtomicBoolean(false);

	protected SnykWizardAuthenticatePage authenticatePage;
	protected IWorkbench workbench;
	protected IStructuredSelection selection;

	// volatile: written on Display thread (createAndLaunch), read on Job thread (doRun).
	private volatile WizardDialog wizardDialog;

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
		return !IN_FLIGHT.get();
	}

	@Override
	public boolean performCancel() {
		IN_FLIGHT.set(false);
		return true;
	}

	@Override
	public boolean performFinish() {
		// Guard against double-click: class-level so multiple wizard instances are also blocked.
		if (!IN_FLIGHT.compareAndSet(false, true)) {
			return false;
		}
		getContainer().updateButtons();

		Job job = new Job("Authenticating with Snyk...") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					return doRun(monitor);
				} finally {
					closeWizard();
				}
			}

			private IStatus doRun(IProgressMonitor monitor) {
				monitor.beginTask("Starting", 3);

				monitor.subTask("Waiting for language server...");
				SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
				// Item 4: include cancel check so the user can abort if LS download stalls.
				while ((SnykStartup.isDownloading() || lc == null) && !monitor.isCanceled()) {
					try {
						Thread.sleep(1000);
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
						return Status.error(e.getMessage());
					}
					lc = SnykExtendedLanguageClient.getInstance();
				}
				if (monitor.isCanceled() || lc == null) {
					return Status.CANCEL_STATUS;
				}
				monitor.worked(1);

				monitor.subTask("Logging out...");
				// logout() sends the command synchronously via executeCommand; the LS
				// processes LSP requests in order, so logout is guaranteed to complete
				// before the login command sent by triggerAuthentication() is handled.
				lc.logout();
				monitor.worked(1);

				monitor.subTask("Opening browser for authentication...");
				final SnykExtendedLanguageClient finalLc = lc;

				// Item 3: open the dialog BEFORE calling triggerAuthentication() so that
				// if the LS responds immediately, closeDialog() is never called on a null
				// reference and the dialog doesn't get left open permanently.
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

				var loginFuture = lc.triggerAuthentication();

				if (waitDialog != null) {
					waitDialog.setCopyUrlEnabled(true);
				}

				monitor.worked(1);
				monitor.done();

				boolean success = false;
				try {
					loginFuture.get(5, TimeUnit.MINUTES);
					success = true;
				} catch (CancellationException e) {
					LOG.info("Authentication cancelled", e);
				} catch (TimeoutException e) {
					LOG.info("Authentication timed out", e);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				} catch (ExecutionException e) {
					LOG.error("Authentication failed", e);
				}

				if (success) {
					trustWorkspaceFoldersClientSide();
					finalLc.updateConfiguration();
				}

				if (waitDialog != null) {
					waitDialog.closeDialog();
				}

				return Status.OK_STATUS;
			}
		};
		try {
			job.schedule();
		} catch (RuntimeException e) { // NOPMD
			IN_FLIGHT.set(false);
			LOG.error("Failed to schedule authentication job", e);
		}

		// Return false so JFace does not auto-close the wizard.
		return false;
	}

	private void trustWorkspaceFoldersClientSide() {
		List<IProject> projects = ResourceUtils.getAccessibleTopLevelProjects();
		if (projects == null || projects.isEmpty()) {
			return;
		}
		String[] paths = projects.stream()
				.filter(p -> p.getLocation() != null && p.getLocation().toFile() != null)
				.map(p -> p.getLocation().toFile().getAbsolutePath())
				.collect(Collectors.toList())
				.toArray(new String[0]);
		TrustedFoldersHelper.addTrustedFolders(paths);
	}

	private void closeWizard() {
		// Reset synchronously so IN_FLIGHT is correct even if Display is disposed.
		IN_FLIGHT.set(false);
		Display display = Display.getDefault();
		if (display != null && !display.isDisposed()) {
			display.asyncExec(() -> {
				if (wizardDialog != null) {
					Shell shell = wizardDialog.getShell();
					if (shell != null && !shell.isDisposed()) {
						wizardDialog.close();
					}
				}
			});
		}
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
