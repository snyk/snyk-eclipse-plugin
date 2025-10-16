package io.snyk.eclipse.plugin.properties;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.concurrent.CompletableFuture;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.preferences.LabelFieldEditor;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;

/**
 * Configure project-specific settings for Snyk
 */
public class ProjectPropertyPage extends FieldEditorPreferencePage implements IWorkbenchPropertyPage {

	public static final String SNYK_ADDITIONAL_PARAMETERS = "snyk.additionalParameters";
	public static final String SNYK_ORGANIZATION = "snyk.projectOrganization";
	public static final String SNYK_AUTO_SELECT_ORG = "snyk.projectOrganizationAutoSelect";
	private IAdaptable element;
	private IProject project;
	private StringFieldEditor additionalParamsEditor;
	private StringFieldEditor projectOrg;
	private Path projectPath;
	private BooleanFieldEditor autoDetectOrgCheckbox;
	private IPreferenceStore preferenceStore = null;

	private class PropertyBooleanFieldEditor extends BooleanFieldEditor {
		public PropertyBooleanFieldEditor(String name, String label, Composite parent) {
			super(name, label, parent);
		}

		@Override
		protected void valueChanged(boolean oldValue, boolean newValue) {
			super.valueChanged(oldValue, newValue);
			var folderConfig = FolderConfigs.getInstance().getFolderConfig(projectPath);
			folderConfig.setOrgSetByUser(!newValue);
			projectOrg.setEnabled(!newValue, getFieldEditorParent());

			if (newValue) {
				// When auto-detect is enabled (checkbox is ticked)
				folderConfig.setPreferredOrg("");
				updateProjectOrg(folderConfig);
			} else {
				// When auto-detect is disabled (checkbox is unticked)
				projectOrg.setStringValue("");
			}
		}

	}

	public ProjectPropertyPage() {
		super(GRID);
	}

	@Override
	public void createFieldEditors() {
		init();
		this.preferenceStore = getPreferenceStore();
		additionalParamsEditor = new StringFieldEditor(SNYK_ADDITIONAL_PARAMETERS, "Additional Parameters:",
				getFieldEditorParent());
		additionalParamsEditor.getTextControl(getFieldEditorParent())
				.setToolTipText("Additional parameters used when calling the CLI, e.g. `-d` or `--exclude=bin`");

		addField(additionalParamsEditor);

		// Auto-select organization checkbox with help icon
		autoDetectOrgCheckbox = new PropertyBooleanFieldEditor(SNYK_AUTO_SELECT_ORG, "Auto-select organization",
				getFieldEditorParent());
		addField(autoDetectOrgCheckbox);

		var url = Preferences.getInstance().getEndpoint() + "/account";
		addField(new LabelFieldEditor(
				"Use automatic organization selection, where the most suitable organization for your project\n"
						+ "is selected from the organizations you have access to which have the project imported, falling\n"
						+ "back to the preferred organization as defined in your web account settings (" + url
						+ ").\n\n"
						+ "If you do not use this feature, then either the org you entered below, or if blank or incorrect, "
						+ "your profile default organization is used.",
				getFieldEditorParent()));

		projectOrg = new StringFieldEditor(SNYK_ORGANIZATION, "Preferred organization:", getFieldEditorParent());
		projectOrg.getTextControl(getFieldEditorParent())
				.setToolTipText("The organization to be used with this project");
		addField(projectOrg);

		populate();
	}

	private void populate() {
		if (!FolderConfigs.LanguageServerConfigReceived.contains(projectPath)) {
			additionalParamsEditor.setEnabled(false, getFieldEditorParent());
			autoDetectOrgCheckbox.setEnabled(false, getFieldEditorParent());
			projectOrg.setEnabled(false, getFieldEditorParent());
		} else {
			var folderConfig = FolderConfigs.getInstance().getFolderConfig(projectPath);
			additionalParamsEditor.setEnabled(true, getFieldEditorParent());
			final var addParams = String.join(" ", folderConfig.getAdditionalParameters());
			if (folderConfig.getAdditionalParameters() != null && !folderConfig.getAdditionalParameters().isEmpty()) {
				preferenceStore.setDefault(SNYK_ADDITIONAL_PARAMETERS, addParams);
				preferenceStore.setValue(SNYK_ADDITIONAL_PARAMETERS, addParams);
				additionalParamsEditor.setStringValue(addParams);
			}

			// Set auto-detect org checkbox (inverse of orgSetByUser)
			autoDetectOrgCheckbox.setEnabled(true, getFieldEditorParent());
			boolean autoDetect = !folderConfig.isOrgSetByUser();
			preferenceStore.setDefault(SNYK_AUTO_SELECT_ORG, autoDetect);
			preferenceStore.setValue(SNYK_AUTO_SELECT_ORG, autoDetect);
			autoDetectOrgCheckbox.load();

			updateProjectOrg(folderConfig);
		}
	}

	private void updateProjectOrg(FolderConfig folderConfig) {
		boolean autoDetect = !folderConfig.isOrgSetByUser();

		String displayOrg;
		if (autoDetect) {
			// When auto-detect is enabled, show the auto-determined organization
			displayOrg = folderConfig.getAutoDeterminedOrg() != null ? folderConfig.getAutoDeterminedOrg() : "";
		} else {
			// When auto-detect is disabled, show the preferred organization
			// If preferred org is empty, show the global organization as fallback
			String preferredOrg = folderConfig.getPreferredOrg() != null ? folderConfig.getPreferredOrg() : "";
			if (preferredOrg.trim().isEmpty()) {
				// Use global organization setting as fallback
				displayOrg = Preferences.getInstance().getPref(Preferences.ORGANIZATION_KEY, "");
			} else {
				displayOrg = preferredOrg;
			}
		}

		preferenceStore.setDefault(SNYK_ORGANIZATION, displayOrg);
		preferenceStore.setValue(SNYK_ORGANIZATION, displayOrg);

		projectOrg.setEnabled(!autoDetect, getFieldEditorParent());
		projectOrg.setStringValue(displayOrg);
	}

	public void init() {
		project = (IProject) getElement().getAdapter(IProject.class);
		if (project == null)
			return;

		projectPath = ResourceUtils.getFullPath(project);
		IScopeContext projectScope = new ProjectScope(project);
		setPreferenceStore(new ScopedPreferenceStore(projectScope, Activator.PLUGIN_ID));
	}

	@Override
	protected void checkState() {
		super.checkState();
		if (!isValid()) {
			return;
		}


		setErrorMessage(null);
		setValid(true);
	}

	@Override
	public boolean performOk() {
		// Validate before saving
		boolean autoDetect = this.autoDetectOrgCheckbox.getBooleanValue();
		// Note: We no longer require organization when auto-detect is disabled
		// because we can fall back to the global organization setting

		var retValue = super.performOk();
		final var addParams = this.additionalParamsEditor.getStringValue().split(" ");

		var folderConfig = FolderConfigs.getInstance().getFolderConfig(projectPath);
		folderConfig.setAdditionalParameters(Arrays.asList(addParams));

		if (autoDetect) {
			// When auto-detect is enabled, set orgSetByUser to false
			folderConfig.setOrgSetByUser(false);
			// Don't change preferredOrg when auto-detect is enabled
		} else {
			// When auto-detect is disabled, set orgSetByUser to true and update preferredOrg
			folderConfig.setOrgSetByUser(true);
			String projectOrgValue = this.projectOrg.getStringValue();
			// Store the project-specific org (can be empty, will fall back to global)
			folderConfig.setPreferredOrg(projectOrgValue != null ? projectOrgValue.trim() : "");
		}

		updateProjectOrg(folderConfig);

		FolderConfigs.getInstance().addFolderConfig(folderConfig);
		CompletableFuture.runAsync(() -> {
			SnykExtendedLanguageClient.getInstance().updateConfiguration();
		});

		return retValue;
	}

	@Override
	public IAdaptable getElement() {
		return this.element;
	}

	@Override
	public void setElement(IAdaptable element) {
		this.element = element;
	}
}
