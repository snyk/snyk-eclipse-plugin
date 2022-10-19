package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.LsConfigurationUpdater;

import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

public class SnykWizardConfigureAdvance extends WizardPage implements Listener {
  private Text organization;
  private Text additionalParameters;
  private Text additionalEnvironment;
  private Button telemetryEnabled;
  private Button errorReportsEnabled;

  public SnykWizardConfigureAdvance() {
    super("Snyk Wizard");
    setTitle("Configure advanced options");
    setDescription("Set advanced options for Snyk.");
  }

  @Override
  public void createControl(Composite parent) {
    Composite composite = new Composite(parent, SWT.NONE);

    GridData gd = new GridData(GridData.FILL_HORIZONTAL);
    GridLayout gl = new GridLayout();
    int ncol = 2;
    gl.numColumns = ncol;
    composite.setLayout(gl);

    Label organizationLabel = new Label(composite, SWT.NONE);
    organizationLabel.setText("Organization:");
    organizationLabel.setToolTipText("Specify the Snyk Organization to use for scanning.");

    organization = new Text(composite, SWT.BORDER);
    organization.setText(Preferences.getInstance().getPref(Preferences.ORGANIZATION_KEY));
    organization.setLayoutData(gd);

    Label additionalParametersLabel = new Label(composite, SWT.NONE);
    additionalParametersLabel.setText("Additional Parameters:");
    additionalParametersLabel
        .setToolTipText("Specify additional parameters to pass to the CLI (for example, --all-projects or -d).");

    additionalParameters = new Text(composite, SWT.BORDER);
    additionalParameters.setText(Preferences.getInstance().getPref(Preferences.ADDITIONAL_PARAMETERS));
    additionalParameters.setLayoutData(gd);

    Label additionalEnvironmentLabel = new Label(composite, SWT.NONE);
    additionalEnvironmentLabel.setText("Additional Environment:");
    additionalEnvironmentLabel.setToolTipText(
        "Add environment variables to Language Server, multiple can be separated by ;. (e.g. JAVA_HOME=/usr/local/bin;GOPATH=/usr/local/bin).");

    additionalEnvironment = new Text(composite, SWT.BORDER);
    additionalEnvironment.setText(Preferences.getInstance().getPref(Preferences.ADDITIONAL_ENVIRONMENT));
    additionalEnvironment.setLayoutData(gd);

    Label telemeteryLabel = new Label(composite, SWT.NONE);
    telemeteryLabel.setText("Send usage statistics to Snyk:");
    telemeteryLabel.setToolTipText("Allow Snyk to get usage data to improve workflows.");

    telemetryEnabled = new Button(composite, SWT.CHECK);
    telemetryEnabled.setSelection(Preferences.getInstance().getBooleanPref(Preferences.ENABLE_TELEMETRY));
    telemetryEnabled.setLayoutData(gd);

    Label errorReportsLabel = new Label(composite, SWT.NONE);
    errorReportsLabel.setText("Send error reports to Snyk:");
    errorReportsLabel.setToolTipText("Send errors from Language Server to Snyk to enable quicker bug fixing.");

    errorReportsEnabled = new Button(composite, SWT.CHECK);
    errorReportsEnabled.setSelection(Preferences.getInstance().getBooleanPref(Preferences.SEND_ERROR_REPORTS));
    errorReportsEnabled.setLayoutData(gd);

    // required to avoid an error in the system
    setControl(composite);
    setPageComplete(false);
  }

  public void handleEvent(Event e) {
    getWizard().getContainer().updateButtons();
  }

  public boolean canFlipToNextPage() {
    return true;
  }

  public IWizardPage getNextPage() {
    updatePreferences();

    SnykWizardSummary page = ((SnykWizard) getWizard()).summary;
    page.onEnterPage();
    return page;
  }

  public void updatePreferences() {
    Preferences.getInstance().store(Preferences.ORGANIZATION_KEY, organization.getText());
    Preferences.getInstance().store(Preferences.ADDITIONAL_PARAMETERS, additionalParameters.getText());
    Preferences.getInstance().store(Preferences.ADDITIONAL_ENVIRONMENT, additionalEnvironment.getText());
    Preferences.getInstance().store(Preferences.ENABLE_TELEMETRY, Boolean.toString(telemetryEnabled.getSelection()));
    Preferences.getInstance().store(Preferences.SEND_ERROR_REPORTS,
        Boolean.toString(errorReportsEnabled.getSelection()));

    new LsConfigurationUpdater().configurationChanged();
  }
}
