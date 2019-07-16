package io.snyk.eclipse.plugin.properties;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;


public class PreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public PreferencesPage() {
		super(GRID);
	}
	
	@Override
	public void init(IWorkbench workbench) {
       setPreferenceStore(Preferences.STORE);
       setMessage("Snyk preferences");
       setDescription("- Please \"authenticate\" this plugin by clicking the button \n- Path is the path to your package managers (npm, maven, etc.) separated by : \n");
	}

	@Override
	protected void createFieldEditors() {
        addField(new AuthButtonFieldEditor(Preferences.AUTH_TOKEN_KEY, "Snyk Auth Token:", getFieldEditorParent()));
        addField(new StringFieldEditor(Preferences.PATH_KEY, "Path:", getFieldEditorParent()));
	}
	
	
}