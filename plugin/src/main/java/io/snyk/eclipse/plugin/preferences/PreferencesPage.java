package io.snyk.eclipse.plugin.preferences;

import java.io.File;
import java.util.concurrent.CompletableFuture;

import org.eclipse.jface.dialogs.MessageDialog;
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
import io.snyk.languageserver.SnykLanguageServer;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class PreferencesPage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	private BooleanFieldEditor snykCodeSecurityCheckbox;
	private BooleanFieldEditor snykCodeQualityCheckbox;

	private static final String PAT_AUTHENTICATION_URL = "https://app.snyk.io/account/personal-access-tokens";
	private static final String TOKEN_AUTHENTICATION_URL = "https://app.snyk.io/account";

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
						+ "Otherwise, for public regional instances, " + "see the docs: ",
				getFieldEditorParent()));
		Link link = new Link(this.getFieldEditorParent(), SWT.NONE);

		link.setText("<a>https://docs.snyk.io/working-with-snyk</a>");
		link.addListener(SWT.Selection, event -> Program.launch(
				"https://docs.snyk.io/working-with-snyk/regional-hosting-and-data-residency#available-snyk-regions"));

		addField(new LabelFieldEditor("For private instances, contact your team or account manager.\n",
				getFieldEditorParent()));

		addField(new StringFieldEditor(Preferences.ENDPOINT_KEY, "Custom Endpoint:", WIDTH, getFieldEditorParent()));
		addField(new BooleanFieldEditor(Preferences.INSECURE_KEY, "Allow unknown certificate authorities",
				getFieldEditorParent()));

		addField(space());

		addField(new LabelFieldEditor(
				"The most secure way to authenticate is using OAuth2. For the alternative authentication methods\n"
						+ "generate a Personal Access Token or a API Token and paste them below",
				getFieldEditorParent()));

		prefs.getInsecureStore().addPropertyChangeListener(storeEvent -> {
			if (Preferences.AUTHENTICATION_METHOD.equals(storeEvent.getProperty())) {
				System.out.println("DEBUG: IPreferenceStore detected change for: " + storeEvent.getProperty()
						+ " New value in store: " + storeEvent.getNewValue());
			}
		});

		// Create the ComboFieldEditor for authentication
		ComboFieldEditor authenticationEditor = new ComboFieldEditor(Preferences.AUTHENTICATION_METHOD,
				"Authentication Method:", AuthConstants.AUTHENTICATION_OPTIONS, getFieldEditorParent());

		// Add the authentication ComboFieldEditor to the field editors
		addField(authenticationEditor);

		Button authenticate = new Button(getFieldEditorParent(), SWT.PUSH);
		authenticate.setText("Get Authentication Token");
		authenticate.addSelectionListener(authenticateSelectionAdapter());

		addField(space());

		TokenFieldEditor tokenField = new TokenFieldEditor(prefs, Preferences.AUTH_TOKEN_KEY,
				"Paste API Token or Peronal Access Token here", getFieldEditorParent());
		addField(space());

//		final var useTokenAuth = new BooleanFieldEditor(Preferences.USE_TOKEN_AUTH,
//				"Use token authentication. It is recommended to keep this turned off, as the default OAuth2 authentication is more secure.",
//				getFieldEditorParent());
//		addField(useTokenAuth);
		addField(tokenField);

		addField(space());
		addField(new LabelFieldEditor("The following options involve the Snyk Language Server.",
				getFieldEditorParent()));
		addField(new LabelFieldEditor(
				"Activating Snyk Code will cause upload of source code to Snyk or the given endpoint address.",
				getFieldEditorParent()));
		addField(space());
		addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_OPEN_SOURCE, "Snyk Open Source enabled",
				getFieldEditorParent()));
		snykCodeSecurityCheckbox = new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_CODE_SECURITY,
				"Snyk Code Security enabled", getFieldEditorParent());
		addField(snykCodeSecurityCheckbox);
		snykCodeQualityCheckbox = new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_CODE_QUALITY,
				"Snyk Code Quality enabled", getFieldEditorParent());
		addField(snykCodeQualityCheckbox);

		addField(new BooleanFieldEditor(Preferences.ACTIVATE_SNYK_IAC, "Snyk Infrastructure-as-Code enabled",
				getFieldEditorParent()));

		addField(space());
		addField(new BooleanFieldEditor(Preferences.SCANNING_MODE_AUTOMATIC, "Scan automatically on start-up and save",
				getFieldEditorParent()));
		addField(space());
		addField(new LabelFieldEditor("Advanced options:", getFieldEditorParent()));
		addField(new StringFieldEditor(Preferences.ORGANIZATION_KEY, "Organization:", WIDTH, getFieldEditorParent()));
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
		disableSnykCodeIfOrgDisabled();
	}

	private SelectionAdapter authenticateSelectionAdapter() {
		return new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				performApply();

				String selectedAuth = getPreferenceStore().getString(Preferences.AUTHENTICATION_METHOD);
				if (AuthConstants.AUTH_PERSONAL_ACCESS_TOKEN.equals(selectedAuth)) {
					boolean launched = Program.launch(PAT_AUTHENTICATION_URL);
					if (!launched) {
						MessageDialog.openError(getShell(), "Open URL failed",
								"Failed to launch PAT authentication URL.");
					}
				} else if (AuthConstants.AUTH_API_TOKEN.equals(selectedAuth)) {
					boolean launched = Program.launch(TOKEN_AUTHENTICATION_URL);
					if (!launched) {
						MessageDialog.openError(getShell(), "Open URL failed",
								"Failed to launch Token authentication URL.");
					}
				}
//				else if (AuthConstants.AUTH_OAUTH2.equals(selectedAuth)) {
//				}
			}
		};
	}

	private FieldEditor space() {
		return new LabelFieldEditor("", getFieldEditorParent());
	}

	@Override
	public boolean performOk() {
		boolean superOK = super.performOk();
		disableSnykCodeIfOrgDisabled();
		CompletableFuture.runAsync(() -> {
			SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();

			lc.updateConfiguration();
			lc.refreshFeatureFlags();
		});
		return superOK;
	}

	private void disableSnykCodeIfOrgDisabled() {
		CompletableFuture.runAsync(() -> {
			SnykLanguageServer.waitForInit();
			boolean isSastEnabled;
			try {
				isSastEnabled = SnykExtendedLanguageClient.getInstance().getSastEnabled();
			} catch (Exception e) {
				SnykLogger.logError(e);
				return;
			}

			String message = "Snyk Code disabled, because it is not enabled for your organization. After you close this preference page, it will stay disabled.";
			final var enabled = isSastEnabled;
			Display.getCurrent().asyncExec(new Runnable() {
				boolean showMessage;
				boolean checkBoxValue;

				@Override
				public void run() {
					checkBoxValue = snykCodeSecurityCheckbox != null && snykCodeSecurityCheckbox.getBooleanValue();
					if (checkBoxValue && !enabled) {
						snykCodeSecurityCheckbox
								.setLabelText(snykCodeSecurityCheckbox.getLabelText() + " (" + message + ")");
						showMessage = true;
					}

					if (showMessage)
						SnykLogger.logInfo(message);
				}
			});
		});
	}

}
