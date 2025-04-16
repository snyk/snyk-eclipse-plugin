package io.snyk.eclipse.plugin.properties;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

/**
 * Configure project-specific settings for Snyk
 */
public class ProjectPropertyPage extends FieldEditorPreferencePage implements IWorkbenchPropertyPage {
	public static final String SNYK_ADDITIONAL_PARAMETERS = "snyk.additionalParameters";
	private IAdaptable element;
	private IProject project = null;
	private StringFieldEditor additionalParamsEditor = null;
	private Path projectPath;

	public ProjectPropertyPage() {
		super(GRID);
	}

	@Override
	public void createFieldEditors() {
		init();		
		additionalParamsEditor = new StringFieldEditor(SNYK_ADDITIONAL_PARAMETERS, "Additional Parameters:",
				getFieldEditorParent());

		addField(additionalParamsEditor);
		
		populate();
	}

	private void populate() {
		if (!FolderConfigs.LanguageServerConfigReceived.contains(projectPath)) {
			additionalParamsEditor.setEnabled(false, getFieldEditorParent());
		} else {
			var folderConfig = FolderConfigs.getInstance().getFolderConfig(projectPath);
			if (folderConfig.getAdditionalParameters() != null && folderConfig.getAdditionalParameters().size()>0) {				
				additionalParamsEditor.setEnabled(true, getFieldEditorParent());
				final var addParams = String.join(" ",  folderConfig.getAdditionalParameters());
				
				final var preferenceStore = getPreferenceStore();
				preferenceStore.setDefault(SNYK_ADDITIONAL_PARAMETERS, addParams);
				preferenceStore.setValue(SNYK_ADDITIONAL_PARAMETERS, addParams);
				
				additionalParamsEditor.setStringValue(addParams);
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
