package io.snyk.eclipse.plugin.properties;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.concurrent.CompletableFuture;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

/**
 * Configure project-specific settings for Snyk
 */
public class ProjectPropertyPage extends FieldEditorPreferencePage implements IWorkbenchPropertyPage {
	public static final String SNYK_ADDITIONAL_PARAMETERS = "snyk.additionalParameters";
	public static final String SNYK_ORGANIZATION = "snyk.projectOrganization";
	private IAdaptable element;
	private IProject project;
	private StringFieldEditor additionalParamsEditor;
	private StringFieldEditor projectOrg;
	private Path projectPath;

	public ProjectPropertyPage() {
		super(GRID);
	}

	@Override
	public void createFieldEditors() {
		init();		
		additionalParamsEditor = new StringFieldEditor(SNYK_ADDITIONAL_PARAMETERS, "Additional Parameters:",
				getFieldEditorParent());
		additionalParamsEditor.getTextControl(getFieldEditorParent()).setToolTipText("Additional parameters used when calling the CLI, e.g. `-d` or `--exclude=bin`");
		
		projectOrg = new StringFieldEditor(SNYK_ORGANIZATION, "Project Organization:",
				getFieldEditorParent());
		projectOrg.getTextControl(getFieldEditorParent()).setToolTipText("The organization to be used with this project");

		addField(additionalParamsEditor);		
		addField(projectOrg);
		populate();
	}

	private void populate() {
		if (!FolderConfigs.LanguageServerConfigReceived.contains(projectPath)) {
			additionalParamsEditor.setEnabled(false, getFieldEditorParent());
			projectOrg.setEnabled(false, getFieldEditorParent());
		} else {
			var folderConfig = FolderConfigs.getInstance().getFolderConfig(projectPath);
				additionalParamsEditor.setEnabled(true, getFieldEditorParent());
				final var addParams = String.join(" ",  folderConfig.getAdditionalParameters());
				final var preferenceStore = getPreferenceStore();				
				if (folderConfig.getAdditionalParameters() != null && !folderConfig.getAdditionalParameters().isEmpty()) {				
					preferenceStore.setDefault(SNYK_ADDITIONAL_PARAMETERS, addParams);
					preferenceStore.setValue(SNYK_ADDITIONAL_PARAMETERS, addParams);
					additionalParamsEditor.setStringValue(addParams);
				}
				
				projectOrg.setEnabled(true, getFieldEditorParent());
				final var preferredOrg = folderConfig.getPreferredOrg();
				if (folderConfig.getPreferredOrg() != null && !folderConfig.getPreferredOrg().isEmpty()) {
					preferenceStore.setDefault(SNYK_ORGANIZATION, preferredOrg);
					preferenceStore.setValue(SNYK_ORGANIZATION, preferredOrg);
					projectOrg.setStringValue(preferredOrg);
				}
		}
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
	public boolean performOk() {
		var retValue = super.performOk();
		final var addParams = this.additionalParamsEditor.getStringValue().split(" ");
		CompletableFuture.runAsync(() -> {
			var folderConfig = FolderConfigs.getInstance().getFolderConfig(projectPath);
			folderConfig.setAdditionalParameters(Arrays.asList(addParams));
			folderConfig.setPreferredOrg(this.projectOrg.getStringValue().trim());
			FolderConfigs.getInstance().addFolderConfig(folderConfig);
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
