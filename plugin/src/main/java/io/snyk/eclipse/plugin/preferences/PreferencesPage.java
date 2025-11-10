package io.snyk.eclipse.plugin.preferences;

import java.io.File;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Link;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class PreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	private BooleanFieldEditor snykCodeSecurityCheckbox;
	private ComboFieldEditor authenticationEditor;
	private StringFieldEditor endpoint;
	private TokenFieldEditor tokenField;
	private BooleanFieldEditor allowInsecureCerts;

	public static final int WIDTH = 60;

	public PreferencesPage() {
		super(GRID);
	}

	@Override
	public void init(IWorkbench workbench) {
		setMessage("Snyk Preferences");
	}

	@Override
	protected void createFieldEditors() {
		final var prefs = Preferences.getInstance();
		// set default store
		setPreferenceStore(prefs.getInsecureStore());

		addField(new LabelFieldEditor(
				"If you're using SSO with Snyk and OAuth2, the custom endpoint configuration is automatically populated.\n"
		                + "Otherwise, for public regional instances, see the docs: ",
				getFieldEditorParent()));
		Link link = new Link(this.getFieldEditorParent(), SWT.NONE);

		link.setText("<a>https://docs.snyk.io/working-with-snyk</a>");
		link.addListener(SWT.Selection, event -> Program.launch(
				"https://docs.snyk.io/working-with-snyk/regional-hosting-and-data-residency#available-snyk-regions"));

		addField(new LabelFieldEditor("For private instances, contact your team or account manager.\n",
				getFieldEditorParent()));

		endpoint = new StringFieldEditor(Preferences.ENDPOINT_KEY, "Custom Endpoint:", WIDTH, getFieldEditorParent());
		addField(endpoint);
		allowInsecureCerts = new BooleanFieldEditor(Preferences.INSECURE_KEY, "Allow unknown certificate authorities",
				getFieldEditorParent());
		addField(allowInsecureCerts);

		addField(space());

		addField(new LabelFieldEditor(
				"The most secure way to authenticate is using OAuth2. For the alternative authentication methods\n"
						+ "generate a Personal Access Token or a API Token and paste them below",
				getFieldEditorParent()));

		authenticationEditor = new ComboFieldEditor(Preferences.AUTHENTICATION_METHOD, "Authentication Method:",
				AuthConstants.getAuthOptions(), getFieldEditorParent());
		addField(authenticationEditor);

		Button authenticate = new Button(getFieldEditorParent(), SWT.PUSH);
		authenticate.setText("Connect IDE to Snyk");
		authenticate.addSelectionListener(authenticateSelectionAdapter());
		addField(space());
		tokenField = new TokenFieldEditor(prefs, Preferences.AUTH_TOKEN_KEY, "API Token or Personal Access Token",
				getFieldEditorParent());
		addField(tokenField);

		addField(space());
		addField(new LabelFieldEditor("The following options involve the Snyk Language Server.",
				getFieldEditorParent()));
		addField(space());
		addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "Snyk Open Source enabled",
				getFieldEditorParent()));
		snykCodeSecurityCheckbox = new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_CODE_SECURITY,
				"Snyk Code Security enabled", getFieldEditorParent());
		addField(snykCodeSecurityCheckbox);
		// Set tooltip on both label and checkbox
		snykCodeSecurityCheckbox.getLabelControl(getFieldEditorParent()).setToolTipText(
				"Code must be enabled for your organization to run.");
		snykCodeSecurityCheckbox.getCheckbox(getFieldEditorParent()).setToolTipText(
				"Code must be enabled for your organization to run.");
		addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_IAC, "Snyk Infrastructure-as-Code enabled",
				getFieldEditorParent()));



		addField(space());
		addField(new BooleanFieldEditor(Preferences.SCANNING_MODE_AUTOMATIC, "Scan automatically on start-up and save",
				getFieldEditorParent()));
		addField(space());
		addField(new LabelFieldEditor("Advanced options:", getFieldEditorParent()));

		// Add label with tooltip text below orgEditor
		addField(new LabelFieldEditor(
            "Specify the organization (ID or name) for Snyk to run scans against.\n"
                    + "Organization selection follows this order:\n"
                    + "1. Project-specific settings (if configured)\n"
                    + "2. This global setting (if the project-specific setting is empty)\n"
                    + "3. Your web account's preferred organization (if both above are empty)\n"
                    + "Manual organization settings override automatic organization selection.",
            getFieldEditorParent()));

        final var orgEditor = new StringFieldEditor(Preferences.ORGANIZATION_KEY, "Organization:", WIDTH, getFieldEditorParent());
		orgEditor.setEnabled(true, getFieldEditorParent());
		orgEditor.getTextControl(getFieldEditorParent()).setToolTipText(
				"Specify the organization (ID or name) for Snyk to run scans against. If the organization is provided manually, automatic organization selection is overridden. If the organization value is blank or invalid, the preferred organization defined in your web account settings will be used.");
		addField(orgEditor);


		addField(new StringFieldEditor(Preferences.ADDITIONAL_PARAMETERS, "Additional Parameters:", WIDTH,
				getFieldEditorParent()));
		addField(new StringFieldEditor(Preferences.ADDITIONAL_ENVIRONMENT, "Additional Environment:", WIDTH,
				getFieldEditorParent()));
		addField(new StringFieldEditor(Preferences.PATH_KEY, "Path:", WIDTH, getFieldEditorParent()));

		addField(space());
		BooleanFieldEditor manageBinaries = new BooleanFieldEditor(Preferences.MANAGE_BINARIES_AUTOMATICALLY,
				"Update and install Snyk binaries automatically", getFieldEditorParent());
		addField(manageBinaries);
		addField(new StringFieldEditor(Preferences.CLI_BASE_URL, "Base URL for CLI download:", WIDTH,
				getFieldEditorParent()));
		addField(
				new FileFieldEditor(Preferences.CLI_PATH, "Snyk CLI (incl. Language Server):", getFieldEditorParent()));

		var releaseChannels = new String[][] { new String[] { "stable", "stable " }, new String[] { "rc", "rc" },
				new String[] { "preview", "preview" } };
		ComboFieldEditor releaseChannelEditor = new ComboFieldEditor(Preferences.RELEASE_CHANNEL,
				"Release Channel or Version:", releaseChannels, getFieldEditorParent());
		addField(releaseChannelEditor);

		addField(space());

		addField(new BooleanFieldEditor(Preferences.SEND_ERROR_REPORTS, "Send error reports to Snyk",
				getFieldEditorParent()));
		addField(new BooleanFieldEditor(Preferences.ENABLE_TELEMETRY, "Send usage statistics to Snyk",
				getFieldEditorParent()));

		addField(space());

		addField(new LabelFieldEditor(
				"Only trusted paths are scanned by Snyk. The Trusted Folders setting allows to specify, which \n"
						+ "paths are safe to scan. Every path below a given path is considered safe to scan. \n"
						+ "Please separate entries with \"" + File.pathSeparator + "\".",
				getFieldEditorParent()));
		StringFieldEditor trustedFoldersEditor = new StringFieldEditor(Preferences.TRUSTED_FOLDERS, "Trusted Folders:",
				WIDTH, getFieldEditorParent());
		addField(trustedFoldersEditor);
	}

	private SelectionAdapter authenticateSelectionAdapter() {
		return new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				endpoint.store();
				authenticationEditor.store();
				allowInsecureCerts.store();
				tokenField.emptyTextfield();
				tokenField.store();
				SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
				lc.updateConfiguration();
				CompletableFuture.runAsync(() -> {
					Object token;
					try {
						token = lc.triggerAuthentication().get(5, TimeUnit.MINUTES);
						Display.getDefault().asyncExec(() -> {
							tokenField.setStringValue(token.toString());
							tokenField.store();
						});
					} catch (InterruptedException e1) {
						Thread.currentThread().interrupt();
					} catch (ExecutionException|TimeoutException e1) {
						SnykLogger.logError(e1);
					}
				});
			};
		};
	}

	private FieldEditor space() {
		return new LabelFieldEditor("", getFieldEditorParent());
	}

	@Override
	public boolean performOk() {
		boolean superOK = super.performOk();
		CompletableFuture.runAsync(() -> {
			SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
			lc.updateConfiguration();
			lc.refreshFeatureFlags();
		});
		return superOK;
	}

}
