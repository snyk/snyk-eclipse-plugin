package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;

import org.eclipse.jface.wizard.IWizardPage;
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

public class SnykWizardConfigureAPI extends WizardPage implements Listener {
  private Text endpoint;
  private Button unknownCerts;
  private String defaultEndpoint = "https://app.snyk.io/api";
  private String initialEndpoint = Preferences.getInstance().getEndpoint();

  public SnykWizardConfigureAPI() {
    super("Snyk Wizard");
    setTitle("Configure Snyk API");
    setDescription("Before scanning your code for vulnerabilities, we must first authenticate with Snyk.");

  }

  @Override
  public void createControl(Composite parent) {
    Composite composite = new Composite(parent, SWT.NONE);
    GridData gd;

    GridLayout gl = new GridLayout();
    int ncol = 2;
    gl.numColumns = ncol;
    composite.setLayout(gl);
    
    Label endpointLabel = new Label(composite, SWT.NONE);
    endpointLabel.setText("Specify the custom endpoint for Single Tenant setups (default: https://app.snyk.io/api):");

    String endpointValue = initialEndpoint.isBlank() ? this.defaultEndpoint : initialEndpoint;
    endpoint = new Text(composite, SWT.BORDER);
    endpoint.setText(endpointValue);
    gd = new GridData(GridData.FILL_HORIZONTAL);
    gd.horizontalSpan = ncol;
    endpoint.setLayoutData(gd);
    
    createLine(composite, ncol);

    Label unknownCertsLabel = new Label(composite, SWT.NONE);
    unknownCertsLabel.setText("Disable certificate checks for SSL connections:");

    unknownCerts = new Button(composite, SWT.CHECK);
    unknownCerts.setSelection(Preferences.getInstance().isInsecure());
    unknownCerts.setLayoutData(gd);

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
    SnykWizardAuthenticate page = ((SnykWizard) getWizard()).authenticate;
    page.onEnterPage();

    return page;
  }

  private void updatePreferences() {
    Preferences.getInstance().store(Preferences.ENDPOINT_KEY, endpoint.getText());
    Preferences.getInstance().store(Preferences.INSECURE_KEY, Boolean.toString(unknownCerts.getSelection()));

    new LsConfigurationUpdater().configurationChanged();
  }

  private void createLine(Composite parent, int ncol) {
    Label line = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.BOLD);
    GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
    gridData.horizontalSpan = ncol;
    line.setLayoutData(gridData);
  }
}
