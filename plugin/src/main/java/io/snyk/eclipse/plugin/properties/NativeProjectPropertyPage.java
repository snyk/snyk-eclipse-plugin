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
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Link;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.preferences.LabelFieldEditor;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;

/**
 * Native project-specific settings page for Snyk (used when HTML settings are disabled)
 */
public class NativeProjectPropertyPage extends FieldEditorPreferencePage implements IWorkbenchPropertyPage {

	public static final String SNYK_ADDITIONAL_PARAMETERS = "snyk.additionalParameters";
	public static final String SNYK_ORGANIZATION = "snyk.projectOrganization";
	public static final String SNYK_AUTO_SELECT_ORG = "snyk.projectOrganizationAutoSelect";
	private IAdaptable element;
	private IProject project;
	private StringFieldEditor additionalParamsEditor;
	private StringFieldEditor projectOrg;
	private Path projectPath;
	private BooleanFieldEditor autoDetectOrgCheckbox;
	private IPreferenceStore preferenceStore;

	private class PropertyBooleanFieldEditor extends BooleanFieldEditor {
		public PropertyBooleanFieldEditor(String name, String label, Composite parent) {
			super(name, label, parent);
		}

		@Override
		protected void valueChanged(boolean oldValue, boolean newValue) {
			super.valueChanged(oldValue, newValue);
			var folderConfig = FolderConfigs.getInstance().getFolderConfig(projectPath);
			if (folderConfig != null) {
				folderConfig.setOrgSetByUser(!newValue);
				projectOrg.setEnabled(!newValue, getFieldEditorParent());

				if (newValue) {
					folderConfig.setPreferredOrg("");
					updateProjectOrg(folderConfig);
				} else {
					projectOrg.setStringValue("");
				}
			}
		}
	}

	public NativeProjectPropertyPage() {
		super(GRID);
	}

	@Override
	protected void createFieldEditors() {
		init();
		this.preferenceStore = getPreferenceStore();
		additionalParamsEditor = new StringFieldEditor(SNYK_ADDITIONAL_PARAMETERS, "Additional Parameters:",
				getFieldEditorParent());
		additionalParamsEditor.getTextControl(getFieldEditorParent())
				.setToolTipText("Additional parameters used when calling the CLI, e.g. `-d` or `--exclude=bin`");

		addField(additionalParamsEditor);

		addField(new LabelFieldEditor("", getFieldEditorParent()));
		addField(new LabelFieldEditor("Organization Settings", getFieldEditorParent()));

		autoDetectOrgCheckbox = new PropertyBooleanFieldEditor(SNYK_AUTO_SELECT_ORG, "Auto-select organization",
				getFieldEditorParent());
		addField(autoDetectOrgCheckbox);

		var url = Preferences.getInstance().getEndpoint() + "/account";
		var urlUserDocs = "https://docs.snyk.io/developer-tools/snyk-ide-plugins-and-extensions/eclipse-plugin/configuration-of-the-eclipse-plugin#global-settings";
		addField(new LabelFieldEditor(
				"[Experimental] Use automatic organization selection. When enabled, Snyk will automatically select the most\n"
						+ "appropriate organization for your project using context found in your repository and your authentication.\n"
						+ "\n"
						+ "If an organization is configured manually, this feature will be overridden.\n"
						+ "\n"
						+ "If an appropriate organization cannot be identified automatically, the preferred organization defined in your\n"
						+ "web account settings will be used as a fallback.",
				getFieldEditorParent()));

		Link webAccountLink = new Link(getFieldEditorParent(), SWT.NONE);
		webAccountLink.setText("<a>web account settings</a>");
		webAccountLink.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				Program.launch(url);
			}
		});

		Link userDocsLink = new Link(getFieldEditorParent(), SWT.NONE);
		userDocsLink.setText("<a>User Docs</a>");
		userDocsLink.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				Program.launch(urlUserDocs);
			}
		});

		projectOrg = new StringFieldEditor(SNYK_ORGANIZATION, "Organization:", getFieldEditorParent());
		addField(projectOrg);

		addField(new LabelFieldEditor(
				"[Experimental] Specify the organization (ID or name) for Snyk to run scans against for this specific IDE project.\n"
						+ "\n"
						+ "If the organization value is blank or invalid, the value from the global Organization field will be used.",
				getFieldEditorParent()));

		Link webAccountLink2 = new Link(getFieldEditorParent(), SWT.NONE);
		webAccountLink2.setText("<a>web account settings</a>");
		webAccountLink2.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				Program.launch(url);
			}
		});

		Link userDocsLink2 = new Link(getFieldEditorParent(), SWT.NONE);
		userDocsLink2.setText("<a>User Docs</a>");
		userDocsLink2.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				Program.launch(urlUserDocs);
			}
		});

		populate();
	}

	private void populate() {
		Composite parent = getFieldEditorParent();

		if (!FolderConfigs.LanguageServerConfigReceived.contains(projectPath)) {
			additionalParamsEditor.setEnabled(false, parent);
			autoDetectOrgCheckbox.setEnabled(false, parent);
			projectOrg.setEnabled(false, parent);

			additionalParamsEditor.setStringValue("");
			projectOrg.setStringValue("");

			preferenceStore.setDefault(SNYK_AUTO_SELECT_ORG, false);
			preferenceStore.setValue(SNYK_AUTO_SELECT_ORG, false);
			autoDetectOrgCheckbox.load();

			preferenceStore.setDefault(SNYK_ORGANIZATION, "");
			preferenceStore.setValue(SNYK_ORGANIZATION, "");
		} else {
			var folderConfig = FolderConfigs.getInstance().getFolderConfig(projectPath);
			additionalParamsEditor.setEnabled(true, parent);

			final var addParams = folderConfig.getAdditionalParameters() != null
					? String.join(" ", folderConfig.getAdditionalParameters())
					: "";
			preferenceStore.setDefault(SNYK_ADDITIONAL_PARAMETERS, addParams);
			preferenceStore.setValue(SNYK_ADDITIONAL_PARAMETERS, addParams);
			additionalParamsEditor.setStringValue(addParams);

			autoDetectOrgCheckbox.setEnabled(true, parent);
			boolean autoDetect = !folderConfig.isOrgSetByUser();
			preferenceStore.setDefault(SNYK_AUTO_SELECT_ORG, autoDetect);
			preferenceStore.setValue(SNYK_AUTO_SELECT_ORG, autoDetect);
			autoDetectOrgCheckbox.load();

			updateProjectOrg(folderConfig);
		}
	}

	private void updateProjectOrg(FolderConfig folderConfig) {
		Composite parent = getFieldEditorParent();

		if (folderConfig == null) {
			projectOrg.setEnabled(false, parent);
			projectOrg.setStringValue("");
			return;
		}

		boolean autoDetect = !folderConfig.isOrgSetByUser();

		String displayOrg;
		if (autoDetect) {
			displayOrg = folderConfig.getAutoDeterminedOrg() != null ? folderConfig.getAutoDeterminedOrg() : "";
		} else {
			displayOrg = folderConfig.getPreferredOrg() != null ? folderConfig.getPreferredOrg() : "";
		}

		displayOrg = displayOrg != null ? displayOrg : "";

		preferenceStore.setDefault(SNYK_ORGANIZATION, displayOrg);
		preferenceStore.setValue(SNYK_ORGANIZATION, displayOrg);

		projectOrg.setEnabled(!autoDetect, parent);
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
		boolean autoDetect = this.autoDetectOrgCheckbox.getBooleanValue();

		var retValue = super.performOk();

		if (!FolderConfigs.LanguageServerConfigReceived.contains(projectPath)) {
			return retValue;
		}

		final var addParams = this.additionalParamsEditor.getStringValue() != null
				? this.additionalParamsEditor.getStringValue().split(" ")
				: new String[0];

		var folderConfig = FolderConfigs.getInstance().getFolderConfig(projectPath);
		if (folderConfig == null) {
			return retValue;
		}

		folderConfig.setAdditionalParameters(Arrays.asList(addParams));

		if (autoDetect) {
			folderConfig.setOrgSetByUser(false);
		} else {
			folderConfig.setOrgSetByUser(true);
			String projectOrgValue = this.projectOrg.getStringValue();
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
