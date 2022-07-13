package io.snyk.eclipse.plugin.properties;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.utils.FileSystemUtil;
import io.snyk.eclipse.plugin.views.DataProvider;
import io.snyk.languageserver.LsConfigurationUpdater;
import io.snyk.languageserver.LsRuntimeEnvironment;
import org.eclipse.jface.dialogs.ControlEnableState;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import java.io.File;

public class PreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

  private static final Preferences PREFERENCES = new Preferences();

  public PreferencesPage() {
    super(GRID);
  }

  @Override
  public void init(IWorkbench workbench) {
    setPreferenceStore(PREFERENCES.getStore());
    setMessage("Snyk Preferences");
  }

  @Override
  protected void createFieldEditors() {
    TokenFieldEditor tokenField = new TokenFieldEditor(PREFERENCES, Preferences.AUTH_TOKEN_KEY, "Snyk API Token:",
      getFieldEditorParent());
    addField(tokenField);
    addField(new StringFieldEditor(Preferences.PATH_KEY, "Path:", getFieldEditorParent()));
    addField(new StringFieldEditor(Preferences.ENDPOINT_KEY, "Custom Endpoint:", getFieldEditorParent()));
    addField(new BooleanFieldEditor(Preferences.INSECURE_KEY, "Allow unknown certificate authorities",
      getFieldEditorParent()));

    addField(space());
    addField(new LabelFieldEditor("The following options are in BETA and involve the language server.", getFieldEditorParent()));
    addField(new LabelFieldEditor("Activating Snyk Code will cause upload of source code to Snyk or the given endpoint address.", getFieldEditorParent()));
    addField(space());
    addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "Snyk Open Source enabled (BETA)",
      getFieldEditorParent()));
    addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_CODE, "Snyk Code enabled (BETA)", getFieldEditorParent()));
    addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_IAC, "Snyk Infrastructure-as-Code enabled (BETA)",
      getFieldEditorParent()));

    addField(space());
    addField(new LabelFieldEditor("Advanced options:", getFieldEditorParent()));
    addField(new StringFieldEditor(Preferences.ORGANIZATION_KEY, "Organization:", getFieldEditorParent()));
    addField(new StringFieldEditor(Preferences.ADDITIONAL_PARAMETERS, "Additional Parameters:", getFieldEditorParent()));
    addField(new StringFieldEditor(Preferences.ADDITIONAL_ENVIRONMENT, "Additional Environment:", getFieldEditorParent()));

    addField(space());
    addField(new BooleanFieldEditor(Preferences.MANAGE_BINARIES_AUTOMATICALLY, "Update and install Snyk binaries automatically", getFieldEditorParent()));
    addField(new FileFieldEditor(Preferences.LS_BINARY_KEY, "Snyk LS Path:", getFieldEditorParent()));
    addField(new FileFieldEditor(Preferences.CLI_PATH, "Snyk CLI Path:", getFieldEditorParent()));

    addField(space());

    addField(new BooleanFieldEditor(Preferences.SEND_ERROR_REPORTS, "Send error reports to Snyk",
      getFieldEditorParent()));
    addField(new BooleanFieldEditor(Preferences.ENABLE_TELEMETRY, "Send usage statistics to Snyk", getFieldEditorParent()));
}

  private FieldEditor space() {
    return new LabelFieldEditor("", getFieldEditorParent());
  }

  @Override
  public boolean performOk() {
    boolean superOK = super.performOk();
    var snykView = SnykStartup.getSnykView();
    snykView.disableRunAbortActions();
    snykView.toggleRunActionEnablement();
    new LsConfigurationUpdater().configurationChanged(PREFERENCES);
    return superOK;
  }

}
