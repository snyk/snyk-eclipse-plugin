package io.snyk.eclipse.plugin.properties;

import java.util.concurrent.CompletableFuture;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbenchPropertyPage;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

/**
 * Configure project-specific settings for Snyk
 */
public class ProjectPropertyPage extends FieldEditorPreferencePage implements IWorkbenchPropertyPage {
	public static final String SNYK_ADDITIONAL_PARAMETERS = "snyk.additionalParameters";
	private IAdaptable element;
	IEclipsePreferences projectNode;

	public ProjectPropertyPage() {
		super(GRID);
	}

	@Override
	public void createFieldEditors() {
		init();
		addField(new StringFieldEditor(SNYK_ADDITIONAL_PARAMETERS, "Additional Parameters:", getFieldEditorParent()));
	}

	public void init() {
		IProject project = (IProject) getElement().getAdapter(IProject.class);
		if (project != null) {
			IScopeContext projectScope = new ProjectScope(project);
			projectNode = projectScope.getNode(Activator.PLUGIN_ID);
			setPreferenceStore(new PreferencesToPreferenceStoreWrapper(projectNode));
		}
	}

	@Override
	public boolean performOk() {
		var retValue = super.performOk();
		CompletableFuture
				.runAsync(() -> SnykExtendedLanguageClient.getInstance().updateConfiguration());
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
