package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.Arrays;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import io.snyk.eclipse.plugin.properties.preferences.FolderConfigs;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class BaseBranchDialog {
	private FolderConfigs preferenceState = FolderConfigs.getInstance();

	public BaseBranchDialog() {
	}

	public void baseBranchDialog(Display display, String projectPath, String[] localBranches) {
		Shell shell = new Shell(display, SWT.APPLICATION_MODAL | SWT.DIALOG_TRIM);
		shell.setText("Choose base branch for net-new issues scanning");
		shell.setLayout(new GridLayout(1, false));
		Label label = new Label(shell, SWT.NONE);
		label.setText("Base Branch for: " + projectPath);
		label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		Combo dropdown = new Combo(shell, SWT.DROP_DOWN);
		dropdown.setItems(localBranches);
		dropdown.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		dropdown.setText(preferenceState.getBaseBranch(projectPath));

		Button okButton = new Button(shell, SWT.PUSH);
		okButton.setText("OK");
		okButton.setLayoutData(new GridData(SWT.END, SWT.CENTER, false, false));
		okButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				// Handle OK button press
				String selectedBranch = dropdown.getText();
				if (Arrays.asList(localBranches).contains(selectedBranch)) {
					preferenceState.setBaseBranch(projectPath, selectedBranch);
					shell.close();
					SnykExtendedLanguageClient.getInstance().triggerScan(projectPath);
				} else {
					SnykLogger.logInfo("Branch is not a valid local branch for repository: " + projectPath);
				}
			}
		});

		shell.pack();
		shell.open();

		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
	}
}