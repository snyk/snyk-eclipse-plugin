package io.snyk.eclipse.plugin.properties;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;


public class SamplePreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public SamplePreferencesPage() {
		super(GRID);
	}
	
	@Override
	public void init(IWorkbench workbench) {
       setPreferenceStore(Preferences.STORE);
       setMessage("Snyk preferences");
       setDescription("- Auth token can be found in you account view \n- Path is the path to you package mangers (npm, maven, etc.) separated by : \n");
	}

	@Override
	protected void createFieldEditors() {
        addField(new StringFieldEditor(Preferences.AUTH_TOKEN_KEY, "Snyk Auth Token:", getFieldEditorParent()));
        addField(new StringFieldEditor(Preferences.PATH_KEY, "Path:", getFieldEditorParent()));
	}
}