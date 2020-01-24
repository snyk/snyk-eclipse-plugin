package io.snyk.eclipse.plugin.properties;


import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.runner.SnykCliRunner;
import io.snyk.eclipse.plugin.views.DataProvider;


public class PreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	
	private SnykCliRunner cliRunner = new SnykCliRunner();
	private AuthButtonFieldEditor tokenField; 
	

	public PreferencesPage() {
		super(GRID);
		Activator.getDefault().getPreferenceStore().addPropertyChangeListener(this::handlePropertyChange);
	}
	
	private void handlePropertyChange(PropertyChangeEvent event) {
		if (event.getProperty().equals(Preferences.ENDPOINT_KEY)) {
			String newEndpoint = event.getNewValue().toString();
			if (newEndpoint.isEmpty()) {
				cliRunner.snykUnsetEndpoint();
			} else {
				cliRunner.snykSetEndpoint(newEndpoint);
			}			
			tokenField.emptyTextfield();
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
		tokenField = new AuthButtonFieldEditor(Preferences.AUTH_TOKEN_KEY, "Snyk API Token:", getFieldEditorParent());
        addField(tokenField);
        addField(new StringFieldEditor(Preferences.PATH_KEY, "Path:", getFieldEditorParent()));        
        addField(space());
        addField(label("Advanced options:"));
        addField(new StringFieldEditor(Preferences.ENDPOINT_KEY, "Custom Endpoint:", getFieldEditorParent()));
        addField(new BooleanFieldEditor(Preferences.INSECURE_KEY, "Allow unknown certificate authorities", getFieldEditorParent()));
	}
	
	
	private FieldEditor space() {
		return new LabelFieldEditor("", getFieldEditorParent());
	}
	
	private FieldEditor label(String label) {
		return new LabelFieldEditor(label, getFieldEditorParent());
	}
	
	public void persist() {
		super.performOk();
	}
	
	@Override
	public boolean performOk() {
		boolean superOK = super.performOk();
		if (tokenField.getStringValue() == null || tokenField.getStringValue().isEmpty()) {
			DataProvider.popUpWarn("Authentication", "Please (re)authenticate before using the Snyk scanner");
		}
		return superOK;
	}
	
}