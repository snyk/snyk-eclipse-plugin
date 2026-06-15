package io.snyk.eclipse.plugin.wizards;

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
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.preferences.Preferences;
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

	// Snapshot of the container shell captured on the UI thread before the Job starts.
	// getContainer() is not thread-safe; we snapshot the Shell so the Job thread can use it.
	private volatile Shell wizardShell;

	public SnykWizard() {
		super();
	}

	@Override
	public String getWindowTitle() {
		return "Snyk Wizard";
	}

	@Override
	public void addPages() {
		addPage(new SnykWizardConfigureAPIPage());
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
		// Cancel any in-flight auth future so the Job thread unblocks immediately
		// instead of waiting up to 30 seconds for the timeout.
		// Do NOT touch IN_FLIGHT — the Job's finally block (closeWizard) owns that reset.
		SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
		if (lc != null) {
			lc.cancelLogin();
		}
		return true;
	}

	@Override
	public boolean performFinish() {
		// Guard against double-click: class-level so multiple wizard instances are also blocked.
		if (!IN_FLIGHT.compareAndSet(false, true)) {
			return false;
		}
		// Snapshot shell on UI thread — getContainer() is not safe to call from Job thread.
		wizardShell = getContainer().getShell();
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
				try {
					// Block until LS acknowledges logout so the subsequent login command
					// is not processed before the session is cleared.
					lc.logout().get(15, TimeUnit.SECONDS);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
					return Status.CANCEL_STATUS;
				} catch (TimeoutException e) {
					LOG.error("Logout timed out; aborting login to avoid session state corruption", e);
					return Status.CANCEL_STATUS;
				} catch (ExecutionException e) {
					LOG.error("Logout failed; aborting login to avoid session state corruption", e);
					return Status.CANCEL_STATUS;
				}
				monitor.worked(1);

				monitor.subTask("Opening browser for authentication...");
				final SnykExtendedLanguageClient finalLc = lc;

				// Open the dialog BEFORE triggerAuthentication() so the dialog is always
				// present when hasAuthenticated fires and closes it.
				AuthWaitDialog[] dialogHolder = new AuthWaitDialog[1];
				Display display = Display.getDefault();
				if (display == null || display.isDisposed()) {
					LOG.warn("Display unavailable; cannot open auth dialog");
					return Status.CANCEL_STATUS;
				}
				display.syncExec(() -> {
					try {
						Shell shell = wizardShell;
						if (shell == null || shell.isDisposed()) {
							LOG.warn("Wizard shell unavailable; auth dialog will not be shown");
							return;
						}
						AuthWaitDialog dialog = new AuthWaitDialog(shell);
						dialog.setOnCancel(finalLc::cancelLogin);
						dialog.setBlockOnOpen(false);
						dialog.open();
						dialogHolder[0] = dialog;
					} catch (RuntimeException e) { // NOPMD
						LOG.error("Failed to open auth dialog", e);
					}
				});

				AuthWaitDialog waitDialog = dialogHolder[0];

				// Push any endpoint/insecure changes from the configure page to the LS before
				// triggering login — LS needs the endpoint to resolve the OAuth provider.
				lc.updateConfiguration();

				var authFuture = lc.triggerAuthentication();

				if (waitDialog != null) {
					waitDialog.setCopyUrlEnabled(true);
				}

				monitor.worked(1);
				monitor.done();

				boolean success = false;
				try {
					authFuture.get(30, TimeUnit.SECONDS);
					// Double-check: hasAuthenticated fires on logout (empty token) too; the future
					// guard above blocks that, but verify the token is actually set as a safety net.
					success = !Preferences.getInstance().getAuthToken().isBlank();
				} catch (CancellationException e) {
					LOG.info("Authentication cancelled", e);
				} catch (TimeoutException e) {
					LOG.info("Authentication timed out", e);
					// hasAuthenticated persists the token before completing the future. If auth
					// landed just after the 30s window, honour the stored token.
					success = !Preferences.getInstance().getAuthToken().isBlank();
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
					LOG.info("Authentication interrupted");
				} catch (ExecutionException e) {
					LOG.error("Authentication failed", e);
				}

				if (waitDialog != null) {
					waitDialog.closeDialog();
				}

				if (success) {
					trustWorkspaceFoldersClientSide();
					finalLc.updateConfiguration();
					return Status.OK_STATUS;
				}
				return Status.CANCEL_STATUS;
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
		try {
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
		} catch (RuntimeException e) { // NOPMD
			LOG.error("Failed to trust workspace folders after authentication", e);
		}
	}

	private void closeWizard() {
		Display display = Display.getDefault();
		if (display == null || display.isDisposed()) {
			// Display gone — safe to reset immediately.
			IN_FLIGHT.set(false);
			return;
		}
		// Reset IN_FLIGHT inside asyncExec so a second wizard cannot enter
		// performFinish() while this wizard's shell is still being closed.
		// Wrap in try-catch: Display can be disposed between the isDisposed() check
		// above and this call (TOCTOU at shutdown). If it throws, reset IN_FLIGHT here
		// so the guard is never permanently stuck.
		try {
		display.asyncExec(() -> {
			try {
				Shell shell = wizardShell;
				if (shell != null && !shell.isDisposed()) {
					shell.close();
				}
			} catch (RuntimeException e) { // NOPMD
				LOG.error("Failed to close wizard dialog", e);
			} finally {
				IN_FLIGHT.set(false);
				// Re-enable Finish in case the same wizard dialog is still open
				// (e.g. shell.close() threw, or this runs before close completes).
				try {
					Shell shell = wizardShell;
					if (shell != null && !shell.isDisposed() && getContainer() != null) {
						getContainer().updateButtons();
					}
				} catch (RuntimeException e) { // NOPMD
					// Wizard already torn down — ignore.
				}
			}
		});
		} catch (SWTException e) {
			// Display was disposed between the isDisposed() check and asyncExec —
			// TOCTOU window at shutdown. Reset the guard so auth can be retried.
			IN_FLIGHT.set(false);
			LOG.warn("Display disposed during wizard close; IN_FLIGHT reset", e);
		}
	}

	public static void createAndLaunch() {
		Display.getDefault().asyncExec(() -> {
			try {
				SnykWizard wizard = new SnykWizard();
				Shell shell = null;
				IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
				if (window != null) {
					shell = window.getShell();
				} else {
					LOG.warn("No active workbench window; auth wizard will open without parent shell");
				}
				WizardDialog dialog = new WizardDialog(shell, wizard);
				dialog.setBlockOnOpen(false);
				dialog.open();
			} catch (RuntimeException e) { // NOPMD
				LOG.error("Failed to open authentication wizard", e);
			}
		});
	}
}
