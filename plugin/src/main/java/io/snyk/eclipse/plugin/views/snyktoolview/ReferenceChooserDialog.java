package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigSettings;
import io.snyk.eclipse.plugin.wizards.SWTWidgetHelper;
import io.snyk.languageserver.LsFolderSettingsKeys;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class ReferenceChooserDialog extends TitleAreaDialog {
	private Path projectPath;
	private Combo branches;
	private Text folderText;

	public ReferenceChooserDialog(Shell parentShell, Path projectPath) {
		super(parentShell);
		this.projectPath = projectPath;
	}

	@Override
	protected boolean isResizable() {
		return true;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite panel = new Composite(parent, SWT.NONE);
		panel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		var gridLayout = new GridLayout();
		gridLayout.numColumns = 1;
		gridLayout.marginBottom = 5;
		gridLayout.marginTop = 5;
		gridLayout.marginLeft = 5;
		gridLayout.marginRight = 5;
		panel.setLayout(gridLayout);
		String pathStr = projectPath.toString();
		FolderConfigSettings configSettings = FolderConfigSettings.getInstance();

		var group = SWTWidgetHelper.createGroup(panel, "Reference branch for " + projectPath.getFileName(), 1,
				GridData.FILL_HORIZONTAL);
		final var localBranches = configSettings.getLocalBranches(pathStr).toArray(new String[0]);
		branches = new Combo(group, SWT.DROP_DOWN);
		branches.setItems(localBranches);
		branches.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		branches.setText(configSettings.getBaseBranch(pathStr));
		branches.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				validate();
			}
		});

		group = SWTWidgetHelper.createGroup(panel, "Reference folder for " + projectPath.getFileName(), 1,
				GridData.FILL_HORIZONTAL);
		folderText = new Text(group, SWT.BORDER);
		folderText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		folderText.setText(configSettings.getReferenceFolderPath(pathStr));
		Button browseButton = new Button(group, SWT.PUSH);
		browseButton.setText("Browse...");

		browseButton.addListener(SWT.Selection, event -> {
			DirectoryDialog dlg = new DirectoryDialog(getShell());
			dlg.setMessage("Select a folder");
			dlg.setFilterPath(folderText.getText());
			String selectedDir = dlg.open();
			validate();
			if (selectedDir != null) {
				setErrorMessage(null);
				folderText.setText(selectedDir);
			}
		});
		
		this.setTitle("Please specify the reference for the net-new issues scan");
		return panel;
	}

	@Override
	public boolean close() {
		if (getErrorMessage() != null)
			return false;

		final var referenceBranch = branches.getText();
		final var referenceFolder = folderText.getText();

		String pathStr = projectPath.toString();
		FolderConfigSettings.getInstance().computeFolderConfig(pathStr, config ->
				config.withSetting(LsFolderSettingsKeys.BASE_BRANCH, referenceBranch, true)
						.withSetting(LsFolderSettingsKeys.REFERENCE_FOLDER_PATH, referenceFolder, true));

		CompletableFuture.runAsync(() -> {
			final var lc = SnykExtendedLanguageClient.getInstance();
			lc.updateConfiguration();
			if (Preferences.getInstance().getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC)) {
				lc.triggerScan(projectPath);
			}
		});

		return super.close();
	}

	@Override
	protected void okPressed() {
		validate();
		super.okPressed();
	}

	private void validate() {
		final var referenceBranch = branches.getText();
		final var referenceFolder = folderText.getText();

		if (referenceBranch.isBlank() && referenceFolder.isBlank()) {
			setErrorMessage("Either a reference branch or a reference folder need to be set.");
		} else {
			setErrorMessage(null);
		}
	}
}