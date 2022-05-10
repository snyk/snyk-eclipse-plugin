package io.snyk.eclipse.plugin.properties;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.runner.SnykCliRunner;
import io.snyk.eclipse.plugin.utils.FileSystemUtil;
import io.snyk.eclipse.plugin.views.DataProvider;
import io.snyk.eclipse.plugin.views.SnykView;
import io.snyk.languageserver.LsConfigurationUpdater;
import io.snyk.languageserver.LsRuntimeEnvironment;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import java.io.File;

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
    setMessage("Snyk Preferences");
  }

  @Override
  protected void createFieldEditors() {
    File lsFile = new LsRuntimeEnvironment(PREFERENCES).getLSFile();
    String msg = "Snyk LS not found at " + lsFile.getAbsolutePath();
    if (lsFile.exists()) {
      msg = "Snyk LS found: " + lsFile.getAbsolutePath();
    }
    addField(new LabelFieldEditor(msg, getFieldEditorParent()));

    File cliFile = FileSystemUtil.getCliFile();
    msg = "CLI not found at " + cliFile.getAbsolutePath();
    if (cliFile.exists()) {
      msg = "CLI found: " + cliFile.getAbsolutePath() + "\n";
    }
    addField(new LabelFieldEditor(msg, getFieldEditorParent()));

    addField(space());

    tokenField = new TokenFieldEditor(PREFERENCES, Preferences.AUTH_TOKEN_KEY, "Snyk API Token:",
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
    addField(new FileFieldEditor(Preferences.LS_BINARY_KEY, "Custom Snyk LS Path:", getFieldEditorParent()));
    addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "Snyk Open Source enabled (BETA)",
      getFieldEditorParent()));
    addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_CODE, "Snyk Code enabled (BETA)", getFieldEditorParent()));
    addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_IAC, "Snyk Infrastructure-as-Code enabled (BETA)",
      getFieldEditorParent()));

    addField(space());
    addField(new LabelFieldEditor("Advanced options:", getFieldEditorParent()));
    addField(new StringFieldEditor(Preferences.ADDITIONAL_PARAMETERS, "Additional Parameters:", getFieldEditorParent()));
    addField(new StringFieldEditor(Preferences.ADDITIONAL_ENVIRONMENT, "Additional Environment:", getFieldEditorParent()));

  }

  private FieldEditor space() {
    return new LabelFieldEditor("", getFieldEditorParent());
  }

  @Override
  public boolean performOk() {
    boolean superOK = super.performOk();
    SnykView snykView = (SnykView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
      .findView(SnykView.ID);
    snykView.disableRunAbortActions();
    if (tokenField.getStringValue() == null || tokenField.getStringValue().isEmpty()) {
      DataProvider.popUpWarn("Authentication", "Please add a valid Snyk Token, else scanning is not possible.");
    }
    snykView.toggleRunActionEnablement();
    new LsConfigurationUpdater().configurationChanged(PREFERENCES);
    return superOK;
  }

}
