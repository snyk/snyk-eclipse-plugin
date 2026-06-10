package io.snyk.eclipse.plugin.wizards;

import java.util.concurrent.CompletableFuture;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;

import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class AuthWaitDialog extends Dialog {

	private static final int COPY_URL_BUTTON_ID = IDialogConstants.CLIENT_ID + 1;

	private Button copyUrlButton;
	private Runnable onCancel;

	public AuthWaitDialog(Shell parentShell) {
		super(parentShell);
	}

	public void setOnCancel(Runnable onCancel) {
		this.onCancel = onCancel;
	}

	@Override
	protected void configureShell(Shell shell) {
		setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
		super.configureShell(shell);
		shell.setText("Authenticating Snyk Plugin");
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite container = (Composite) super.createDialogArea(parent);
		container.setLayout(new GridLayout(1, false));
		container.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		Label message = new Label(container, SWT.WRAP);
		message.setText(
				"We are now redirecting you to our auth page, go ahead and log in.\n\n"
				+ "Once the authentication is complete, return to the IDE and you'll be ready to start using Snyk.\n\n"
				+ "If a browser window doesn't open after a few seconds, please copy the URL "
				+ "using the button below and manually paste it in a browser.");
		GridData msgData = new GridData(SWT.FILL, SWT.FILL, true, true);
		msgData.widthHint = 450;
		message.setLayoutData(msgData);

		ProgressBar progressBar = new ProgressBar(container, SWT.INDETERMINATE);
		progressBar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		return container;
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		copyUrlButton = createButton(parent, COPY_URL_BUTTON_ID, "Copy URL", false);
		copyUrlButton.setEnabled(false);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == COPY_URL_BUTTON_ID) {
			copyUrlButton.setEnabled(false);
			Display display = Display.getDefault();
			// Fetch auth link off the UI thread to avoid blocking SWT event dispatch.
			CompletableFuture.supplyAsync(() -> SnykExtendedLanguageClient.getInstance().getAuthLink())
					.thenAccept(url -> display.asyncExec(() -> {
						if (copyUrlButton != null && !copyUrlButton.isDisposed()) {
							copyUrlButton.setEnabled(true);
						}
						Shell shell = getShell();
						if (shell == null || shell.isDisposed()) {
							return;
						}
						if (!url.isBlank()) {
							Clipboard clipboard = new Clipboard(display);
							try {
								clipboard.setContents(new Object[]{url}, new Transfer[]{TextTransfer.getInstance()});
							} finally {
								clipboard.dispose();
							}
						}
					}));
			return;
		}
		if (buttonId == IDialogConstants.CANCEL_ID) {
			if (onCancel != null) {
				onCancel.run();
			}
			cancelPressed();
			return;
		}
		super.buttonPressed(buttonId);
	}

	public void setCopyUrlEnabled(boolean enabled) {
		Display.getDefault().asyncExec(() -> {
			if (copyUrlButton != null && !copyUrlButton.isDisposed()) {
				copyUrlButton.setEnabled(enabled);
			}
		});
	}

	public void closeDialog() {
		Display.getDefault().asyncExec(() -> {
			Shell shell = getShell();
			if (shell != null && !shell.isDisposed()) {
				close();
			}
		});
	}
}
