package io.snyk.eclipse.plugin.properties;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.runner.SnykCliRunner;
import io.snyk.eclipse.plugin.utils.FileSystemUtil;
import io.snyk.eclipse.plugin.views.DataProvider;
import io.snyk.eclipse.plugin.views.SnykView;
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
    setMessage("Snyk preferences");
    setDescription("- Please enter the token in the token field, you can get it from https://app.snyk.io \n"
      + "- Specify custom location of Snyk Language Server and disable updates in Custom Snyk LS Path\n"
      + "- Path entries are added to the OS path\n" + "Custom Endpoint only needed for on-prem solution \n");
  }

  @Override
  protected void createFieldEditors() {
    File lsFile = new LsRuntimeEnvironment(PREFERENCES).getLSFile();
    String msg = "Snyk LS not found at " + lsFile.getAbsolutePath();
    if (lsFile.exists()) {
      msg = "Snyk LS found: " + lsFile.getAbsolutePath() + "\n";
    }
    File cliFile = FileSystemUtil.getCliFile();
    if (cliFile.exists()) {
      msg += "CLI found: " + cliFile.getAbsolutePath() + "\n";
    }

    LabelFieldEditor labelEditor = new LabelFieldEditor(msg, getFieldEditorParent());
    addField(labelEditor);

    addField(new FileFieldEditor(Preferences.LS_BINARY_KEY, "Custom Snyk LS Path:", getFieldEditorParent()));
    tokenField = new TokenFieldEditor(PREFERENCES, Preferences.AUTH_TOKEN_KEY, "Snyk API Token:",
      getFieldEditorParent());
    addField(tokenField);
    addField(new StringFieldEditor(Preferences.PATH_KEY, "Path:", getFieldEditorParent()));
    addField(space());
    addField(new LabelFieldEditor("Advanced options:", getFieldEditorParent()));
    addField(new StringFieldEditor(Preferences.ENDPOINT_KEY, "Custom Endpoint:", getFieldEditorParent()));
    addField(new BooleanFieldEditor(Preferences.INSECURE_KEY, "Allow unknown certificate authorities",
      getFieldEditorParent()));
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
    return superOK;
  }

}
