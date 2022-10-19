package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

public class SnykWizardSummary extends WizardPage implements Listener {
  private Button manageDependenciesEnabled;
  private Text path;
  private Text languageServer;
  private Text cli;
  
  private Text endpoint;
  private Button unknownCerts;
  
  private Button openSourceEnabled;
  private Button codeEnabled;
  private Button iacEnabled;
  
  private Text organization;
  private Text additionalParameters;
  private Text additionalEnvironment;
  private Button errorReportsEnabled;
  private Button telemetryEnabled;
  
  private Button authenticate;

  public SnykWizardSummary() {
    super("Snyk Wizard");
    setTitle("Here's a summary of your Snyk settings.");
  }

  @Override
  public void createControl(Composite parent) {
    Composite composite = new Composite(parent, SWT.NONE);
    GridLayout gl = new GridLayout();
    GridData gd = new GridData(GridData.FILL_HORIZONTAL);
    
    int ncol = 2;
    gl.numColumns = ncol;
    composite.setLayout(gl);

    // Snyk Dependencies
    Label manageDependenciesLabel = new Label(composite, SWT.NONE);
    manageDependenciesLabel.setText("Update and install Snyk binaries automatically:");
    manageDependenciesLabel.setToolTipText(
        "If disabled, no updates are downloaded and updates must be performed manually. Please make sure that the locations for Language Server and CLI point to an existent, current binary.");

    manageDependenciesEnabled = new Button(composite, SWT.CHECK);
    manageDependenciesEnabled.setLayoutData(gd);

    Label pathLabel = new Label(composite, SWT.NONE);
    pathLabel.setText("Path:");
    pathLabel
        .setToolTipText("Specify your additions to the path to find needed third party tools such as Gradle or Maven.");

    path = new Text(composite, SWT.BORDER | SWT.READ_ONLY);
    path.setLayoutData(gd);

    Label languageServerLabel = new Label(composite, SWT.NONE);
    languageServerLabel.setText("Snyk Language Server:");
    languageServerLabel.setToolTipText("Specify the location of your language server binary.");

    languageServer = new Text(composite, SWT.BORDER | SWT.READ_ONLY);
    languageServer.setLayoutData(gd);

    Label cliLabel = new Label(composite, SWT.NONE);
    cliLabel.setText("Snyk CLI:");
    cliLabel.setToolTipText("Specify the location of the Snyk CLI.");

    cli = new Text(composite, SWT.BORDER | SWT.READ_ONLY);
    cli.setLayoutData(gd);

    // Snyk API
    Label endpointLabel = new Label(composite, SWT.NONE);
    endpointLabel.setText("Custom Endpoint:");
    endpointLabel
        .setToolTipText("Specify the custom endpoint for Single Tenant setups instead of https://app.snyk.io.");

    endpoint = new Text(composite, SWT.BORDER | SWT.READ_ONLY);
    endpoint.setLayoutData(gd);

    Label unknownCertsLabel = new Label(composite, SWT.NONE);
    unknownCertsLabel.setText("Allow unknown certificate authorities:");
    unknownCertsLabel.setToolTipText("Disable certificate checks for SSL connections.");

    unknownCerts = new Button(composite, SWT.CHECK);
    unknownCerts.setLayoutData(gd);

    // Snyk Products
    Label openSourceLabel = new Label(composite, SWT.NONE);
    openSourceLabel.setText("Snyk Open Source enabled:");
    openSourceLabel.setToolTipText("Enable/Disable Snyk Open Source Dependency Scans via Language Server.");

    openSourceEnabled = new Button(composite, SWT.CHECK);
    openSourceEnabled.setLayoutData(gd);

    Label codeLabel = new Label(composite, SWT.NONE);
    codeLabel.setText("Snyk Code enabled:");
    codeLabel.setToolTipText("Enable/Disable Snyk Code Scans via Language Server.");

    codeEnabled = new Button(composite, SWT.CHECK);
    codeEnabled.setLayoutData(gd);

    Label iacLabel = new Label(composite, SWT.NONE);
    iacLabel.setText("Snyk Infrastructure-as-Code enabled:");
    iacLabel.setToolTipText("Enable/Disable Snyk IaC Scans via Language Server.");

    iacEnabled = new Button(composite, SWT.CHECK);
    iacEnabled.setLayoutData(gd);

    // Snyk Advance
    Label organizationLabel = new Label(composite, SWT.NONE);
    organizationLabel.setText("Organization:");
    organizationLabel.setToolTipText("Specify the Snyk Organization to use for scanning.");

    organization = new Text(composite, SWT.BORDER | SWT.READ_ONLY);
    organization.setLayoutData(gd);

    Label additionalParametersLabel = new Label(composite, SWT.NONE);
    additionalParametersLabel.setText("Additional Parameters:");
    additionalParametersLabel
        .setToolTipText("Specify additional parameters to pass to the CLI (for example, --all-projects or -d).");

    additionalParameters = new Text(composite, SWT.BORDER | SWT.READ_ONLY);
    additionalParameters.setLayoutData(gd);

    Label additionalEnvironmentLabel = new Label(composite, SWT.NONE);
    additionalEnvironmentLabel.setText("Additional Environment:");
    additionalEnvironmentLabel.setToolTipText(
        "Add environment variables to Language Server, multiple can be separated by ;. (e.g. JAVA_HOME=/usr/local/bin;GOPATH=/usr/local/bin).");

    additionalEnvironment = new Text(composite, SWT.BORDER | SWT.READ_ONLY);
    additionalEnvironment.setLayoutData(gd);

    Label telemeteryLabel = new Label(composite, SWT.NONE);
    telemeteryLabel.setText("Send usage statistics to Snyk:");
    telemeteryLabel.setToolTipText("Allow Snyk to get usage data to improve workflows.");

    telemetryEnabled = new Button(composite, SWT.CHECK);
    telemetryEnabled.setLayoutData(gd);

    Label errorReportsLabel = new Label(composite, SWT.NONE);
    errorReportsLabel.setText("Send error reports to Snyk:");
    errorReportsLabel.setToolTipText("Send errors from Language Server to Snyk to enable quicker bug fixing.");

    errorReportsEnabled = new Button(composite, SWT.CHECK);
    errorReportsEnabled.setLayoutData(gd);
    
    // Authenticate
    authenticate = new Button(composite, SWT.PUSH);
    authenticate.setText("Authenticate");
    authenticate.setToolTipText("Authenticate with the endpoint defined above.");
    authenticate.addListener(SWT.Selection, this);
    gd = new GridData();
    gd.horizontalAlignment = GridData.BEGINNING;
    authenticate.setLayoutData(gd);

    // required to avoid an error in the system
    setControl(composite);
    setPageComplete(false);
  }

  public void handleEvent(Event e) {
    if (e.widget == authenticate) {
      SnykExtendedLanguageClient.getInstance().triggerAuthentication();
    }
    
    getWizard().getContainer().updateButtons();
  }

  public boolean isPageComplete() {
    return true;
  }
  
  void onEnterPage() {
    manageDependenciesEnabled.setSelection(Preferences.getInstance().getBooleanPref(Preferences.MANAGE_BINARIES_AUTOMATICALLY));
    path.setText(Preferences.getInstance().getPref(Preferences.PATH_KEY));
    languageServer.setText(Preferences.getInstance().getPref(Preferences.LS_BINARY_KEY));
    cli.setText(Preferences.getInstance().getPref(Preferences.CLI_PATH));
    
    endpoint.setText(Preferences.getInstance().getEndpoint());
    unknownCerts.setSelection(Preferences.getInstance().getBooleanPref(Preferences.INSECURE_KEY));
    
    openSourceEnabled.setSelection(Preferences.getInstance().getBooleanPref(Preferences.ACTIVATE_SNYK_OPEN_SOURCE));
    codeEnabled.setSelection(Preferences.getInstance().getBooleanPref(Preferences.ACTIVATE_SNYK_CODE));
    iacEnabled.setSelection(Preferences.getInstance().getBooleanPref(Preferences.ACTIVATE_SNYK_IAC));
    
    organization.setText(Preferences.getInstance().getPref(Preferences.ORGANIZATION_KEY));
    additionalParameters.setText(Preferences.getInstance().getPref(Preferences.ADDITIONAL_PARAMETERS));
    additionalEnvironment.setText(Preferences.getInstance().getPref(Preferences.ADDITIONAL_ENVIRONMENT));
    telemetryEnabled.setSelection(Preferences.getInstance().getBooleanPref(Preferences.ENABLE_TELEMETRY));
    errorReportsEnabled.setSelection(Preferences.getInstance().getBooleanPref(Preferences.SEND_ERROR_REPORTS));
  }
}
