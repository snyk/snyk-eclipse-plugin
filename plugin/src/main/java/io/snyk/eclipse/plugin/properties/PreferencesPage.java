package io.snyk.eclipse.plugin.properties;

import java.io.File;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.properties.preferences.ApiClient;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.LsConfigurationUpdater;

public class PreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	private BooleanFieldEditor snykCodeCheckbox;

	public PreferencesPage() {
		super(GRID);
	}

	@Override
	public void init(IWorkbench workbench) {
		setPreferenceStore(io.snyk.eclipse.plugin.properties.preferences.Preferences.getInstance().getStore());
		setMessage("Snyk Preferences");
	}

	@Override
	protected void createFieldEditors() {
		TokenFieldEditor tokenField = new TokenFieldEditor(
				io.snyk.eclipse.plugin.properties.preferences.Preferences.getInstance(),
				io.snyk.eclipse.plugin.properties.preferences.Preferences.AUTH_TOKEN_KEY, "Snyk API Token:",
				getFieldEditorParent());
		addField(tokenField);
		addField(new StringFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.PATH_KEY, "Path:",
				getFieldEditorParent()));
		addField(new StringFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.ENDPOINT_KEY,
				"Custom Endpoint:", getFieldEditorParent()));
		addField(new BooleanFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.INSECURE_KEY,
				"Allow unknown certificate authorities", getFieldEditorParent()));

		addField(space());
		addField(new LabelFieldEditor("The following options involve the Snyk Language Server.",
				getFieldEditorParent()));
		addField(new LabelFieldEditor(
				"Activating Snyk Code will cause upload of source code to Snyk or the given endpoint address.",
				getFieldEditorParent()));
		addField(space());
		addField(new BooleanFieldEditor(
				io.snyk.eclipse.plugin.properties.preferences.Preferences.ACTIVATE_SNYK_OPEN_SOURCE,
				"Snyk Open Source enabled", getFieldEditorParent()));
		snykCodeCheckbox = new BooleanFieldEditor(
				io.snyk.eclipse.plugin.properties.preferences.Preferences.ACTIVATE_SNYK_CODE, "Snyk Code enable" + "d",
				getFieldEditorParent());

		addField(snykCodeCheckbox);
		addField(new BooleanFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.ACTIVATE_SNYK_IAC,
				"Snyk Infrastructure-as-Code enabled", getFieldEditorParent()));

		addField(space());
		addField(new LabelFieldEditor("Advanced options:", getFieldEditorParent()));
		addField(new StringFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.ORGANIZATION_KEY,
				"Organization:", getFieldEditorParent()));
		addField(new StringFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.ADDITIONAL_PARAMETERS,
				"Additional Parameters:", getFieldEditorParent()));
		addField(new StringFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.ADDITIONAL_ENVIRONMENT,
				"Additional Environment:", getFieldEditorParent()));

		addField(space());
		BooleanFieldEditor manageBinaries = new BooleanFieldEditor(Preferences.MANAGE_BINARIES_AUTOMATICALLY,
				"Update and install Snyk binaries automatically", getFieldEditorParent());
		manageBinaries.setPropertyChangeListener((PropertyChangeEvent propertyChangeEvent) -> {
			System.out.println("managed bionaries changed");
		});
		addField(manageBinaries);
		addField(new FileFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.LS_BINARY_KEY,
				"Snyk Language Server:", getFieldEditorParent()));
		addField(new FileFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.CLI_PATH, "Snyk CLI:",
				getFieldEditorParent()));

		addField(space());

		addField(new BooleanFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.SEND_ERROR_REPORTS,
				"Send error reports to Snyk", getFieldEditorParent()));
		addField(new BooleanFieldEditor(Preferences.ENABLE_TELEMETRY, "Send usage statistics to Snyk",
				getFieldEditorParent()));

		addField(space());

		addField(new LabelFieldEditor(
				  "Only trusted paths are scanned by Snyk. The Trusted Folders setting allows to specify, which \n"
				+ "paths are safe to scan. Every path below a given path is considered safe to scan. \n"
				+ "Please separate entries with \"" + File.pathSeparator + "\".",
				getFieldEditorParent()));
		addField(new StringFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences.TRUSTED_FOLDERS,
				"Trusted Folders:", getFieldEditorParent()));

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
		var apiClient = new ApiClient();
		if (snykCodeCheckbox.getBooleanValue() && !apiClient.checkSnykCodeEnablement()) {
			String message = "Snyk Code disabled, because it is not enabled for your organization. After you close this preference page, it will stay disabled.";
			snykCodeCheckbox.setLabelText(snykCodeCheckbox.getLabelText() + " (" + message + ")");
			SnykLogger.logInfo(message);
		}
	}

}
