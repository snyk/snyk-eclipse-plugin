package io.snyk.eclipse.plugin.properties;

import java.io.File;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsConfigurationUpdater;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class PreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
    private BooleanFieldEditor snykCodeSecurityCheckbox, snykCodeQualityCheckbox;

    public PreferencesPage() {
        super(GRID);
    }

    @Override
    public void init(IWorkbench workbench) {
        setPreferenceStore(Preferences.getInstance().getStore());
        setMessage("Snyk Preferences");
    }

    @Override
    protected void createFieldEditors() {
        TokenFieldEditor tokenField = new TokenFieldEditor(Preferences.getInstance(), Preferences.AUTH_TOKEN_KEY,
                "Token:", getFieldEditorParent());

        addField(new BooleanFieldEditor(Preferences.USE_TOKEN_AUTH,
                "Use token authentication. It is recommended to keep this turned off, as the default OAuth2 authentication is more secure.",
                getFieldEditorParent()));

        addField(tokenField);
        addField(new StringFieldEditor(Preferences.PATH_KEY, "Path:", 80, getFieldEditorParent()));
        addField(new StringFieldEditor(Preferences.ENDPOINT_KEY, "Custom Endpoint:", 80, getFieldEditorParent()));
        addField(new BooleanFieldEditor(Preferences.INSECURE_KEY, "Allow unknown certificate authorities",
                getFieldEditorParent()));

        addField(space());
        addField(new LabelFieldEditor("The following options involve the Snyk Language Server.",
                getFieldEditorParent()));
        addField(new LabelFieldEditor(
                "Activating Snyk Code will cause upload of source code to Snyk or the given endpoint address.",
                getFieldEditorParent()));
        addField(space());
        addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "Snyk Open Source enabled",
                getFieldEditorParent()));
        snykCodeSecurityCheckbox = new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_CODE_SECURITY, "Snyk Code Security enabled",
                getFieldEditorParent());
        addField(snykCodeSecurityCheckbox);
        snykCodeQualityCheckbox = new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_CODE_QUALITY, "Snyk Code Quality enabled",
                getFieldEditorParent());
        addField(snykCodeQualityCheckbox);

        addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_IAC, "Snyk Infrastructure-as-Code enabled",
                getFieldEditorParent()));

        addField(space());
        addField(new BooleanFieldEditor(Preferences.SCANNING_MODE_AUTOMATIC, "Scan automatically on start-up and save",
                getFieldEditorParent()));
        addField(space());
        addField(new LabelFieldEditor("Advanced options:", getFieldEditorParent()));
        addField(new StringFieldEditor(Preferences.ORGANIZATION_KEY, "Organization:", 80, getFieldEditorParent()));
        addField(
                new StringFieldEditor(Preferences.ADDITIONAL_PARAMETERS, "Additional Parameters:", 80,
                        getFieldEditorParent()));
        addField(
                new StringFieldEditor(Preferences.ADDITIONAL_ENVIRONMENT, "Additional Environment:", 80,
                        getFieldEditorParent()));

        addField(space());
        BooleanFieldEditor manageBinaries = new BooleanFieldEditor(Preferences.MANAGE_BINARIES_AUTOMATICALLY,
                "Update and install Snyk binaries automatically", getFieldEditorParent());
        manageBinaries.setPropertyChangeListener((PropertyChangeEvent propertyChangeEvent) -> {
            System.out.println("managed bionaries changed");
        });
        addField(manageBinaries);
        addField(new StringFieldEditor(Preferences.CLI_BASE_URL, "Base URL for CLI download:", 80,
                getFieldEditorParent()));
        addField(
                new FileFieldEditor(Preferences.CLI_PATH, "Snyk CLI (incl. Language Server):", getFieldEditorParent()));

        addField(space());

        addField(
                new BooleanFieldEditor(Preferences.SEND_ERROR_REPORTS, "Send error reports to Snyk",
                        getFieldEditorParent()));
        addField(
                new BooleanFieldEditor(Preferences.ENABLE_TELEMETRY, "Send usage statistics to Snyk",
                        getFieldEditorParent()));

        addField(space());

        addField(new LabelFieldEditor(
                "Only trusted paths are scanned by Snyk. The Trusted Folders setting allows to specify, which \n"
                        + "paths are safe to scan. Every path below a given path is considered safe to scan. \n"
                        + "Please separate entries with \"" + File.pathSeparator + "\".",
                getFieldEditorParent()));
        StringFieldEditor trustedFoldersEditor = new StringFieldEditor(Preferences.TRUSTED_FOLDERS, "Trusted Folders:",
                80, getFieldEditorParent());
        addField(trustedFoldersEditor);
        disableSnykCodeIfOrgDisabled();
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
        disableSnykCodeIfOrgDisabled();

        new LsConfigurationUpdater().configurationChanged();
        return superOK;
    }

    private void disableSnykCodeIfOrgDisabled() {
        Display.getCurrent().asyncExec(new Runnable() {
            @Override
            public void run() {
                boolean isSastEnabled = false;
                try {
                    isSastEnabled = SnykExtendedLanguageClient.getInstance().getSastEnabled();
                } catch (Exception e) {
                    SnykLogger.logError(e);
                    return;
                }
                String message = "Snyk Code disabled, because it is not enabled for your organization. After you close this preference page, it will stay disabled.";
                boolean showMessage = false;
                boolean checkBoxValue;
                try {
                    checkBoxValue = snykCodeSecurityCheckbox != null && snykCodeSecurityCheckbox.getBooleanValue();
                } catch (NullPointerException e) {
                    // this can happen, if the UI checkbox is not initialized fully, we return then
                    return;
                }
                if (checkBoxValue && !isSastEnabled) {
                    snykCodeSecurityCheckbox
                            .setLabelText(snykCodeSecurityCheckbox.getLabelText() + " (" + message + ")");
                    showMessage = true;
                }

                if (showMessage)
                    SnykLogger.logInfo(message);
            }
        });
    }

}
