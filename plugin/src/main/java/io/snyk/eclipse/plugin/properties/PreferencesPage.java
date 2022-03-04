package io.snyk.eclipse.plugin.properties;


import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.runner.SnykCliRunner;
import io.snyk.eclipse.plugin.views.DataProvider;
import org.eclipse.jface.preference.*;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;


public class PreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

    private static final Preferences PREFERENCES = new Preferences();

    private final SnykCliRunner cliRunner = new SnykCliRunner();
    private TokenFieldEditor tokenField;


    public PreferencesPage() {
        super(GRID);
        Activator.getDefault().getPreferenceStore().addPropertyChangeListener(this::handlePropertyChange);
    }

    private void handlePropertyChange(PropertyChangeEvent event) {
        // don't run 'snyk config' command, so we use settings from eclipse prefs only
//		if (event.getProperty().equals(Preferences.ENDPOINT_KEY)) {
//			String newEndpoint = event.getNewValue().toString();
//			if (newEndpoint.isEmpty()) {
//				cliRunner.snykUnsetEndpoint();
//			} else {
//				cliRunner.snykSetEndpoint(newEndpoint);
//			}
//			tokenField.emptyTextfield();
//		}
    }

    @Override
    public void init(IWorkbench workbench) {
        setPreferenceStore(PREFERENCES.getStore());
        setMessage("Snyk preferences");
        setDescription("- Please enter the token in the token field, you can get it from https://app.snyk.io \n"
                + "- Snyk LS Path allows you to set a custom location for the Snyk Language Server \n"
                + "- Path is the path to your package managers (npm, maven, etc.) separated by : \n- Custom Endpoint only needed for on-prem solution \n");
    }

    @Override
    protected void createFieldEditors() {
        addField(new FileFieldEditor(Preferences.LS_BINARY_KEY, "Snyk LS Path:", getFieldEditorParent()));
        tokenField = new TokenFieldEditor(PREFERENCES, Preferences.AUTH_TOKEN_KEY, "Snyk API Token:", getFieldEditorParent());
        addField(tokenField);
        addField(new StringFieldEditor(Preferences.PATH_KEY, "Path:", getFieldEditorParent()));
        addField(space());
        addField(new LabelFieldEditor("Advanced options:", getFieldEditorParent()));
        addField(new StringFieldEditor(Preferences.ENDPOINT_KEY, "Custom Endpoint:", getFieldEditorParent()));
        addField(new BooleanFieldEditor(Preferences.INSECURE_KEY, "Allow unknown certificate authorities", getFieldEditorParent()));
    }


    private FieldEditor space() {
        return new LabelFieldEditor("", getFieldEditorParent());
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