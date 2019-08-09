package io.snyk.eclipse.plugin.properties;


import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.runner.ProcessResult;
import io.snyk.eclipse.plugin.runner.SnykCliRunner;


public class PreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	
	private SnykCliRunner cliRunner = new SnykCliRunner();
	private Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

	public PreferencesPage() {
		super(GRID);
		Activator.getDefault().getPreferenceStore().addPropertyChangeListener(this::handlePropertyChange);
	}
	
	private void handlePropertyChange(PropertyChangeEvent event) {
		if (event.getProperty().equals(Preferences.ENDPOINT_KEY)) {
			String newEndpoint = event.getNewValue().toString();
			String okMessage = "Custom endpoint configuration set to: " + newEndpoint + "\nPlease (re)authenticate for this endpoint";
			if (newEndpoint.isEmpty()) okMessage = "Custom endpoint removed \nPlease (re)authenticate";
			handleProcessResult(cliRunner.snykSetEndpoint(newEndpoint), "Custom endpoint configuration failed", okMessage);
		}
	}

	@Override
	public void init(IWorkbench workbench) {
       setPreferenceStore(Preferences.STORE);
       setMessage("Snyk preferences");
       setDescription("- Please authenticate this plugin by clicking the button \n- Path is the path to your package managers (npm, maven, etc.) separated by : \n- Custom Endpoint only needed for on-prem solution \n");
	}

	@Override
	protected void createFieldEditors() {
        addField(new AuthButtonFieldEditor(Preferences.AUTH_TOKEN_KEY, "Snyk Auth Token:", getFieldEditorParent()));
        addField(new StringFieldEditor(Preferences.PATH_KEY, "Path:", getFieldEditorParent()));
        addField(new StringFieldEditor(Preferences.ENDPOINT_KEY, "Custom Endpoint:", getFieldEditorParent()));
	}
	

	
	private void handleProcessResult(ProcessResult result, String errorMessage, String okMessage) {
		if (result.hasErrorOrContentError()) shell.getDisplay().asyncExec(()-> MessageDialog.openError(shell, errorMessage, result.getErrorOrContent()));
		else shell.getDisplay().asyncExec(()-> MessageDialog.openInformation(shell, "", okMessage));		
	}
	
	
}