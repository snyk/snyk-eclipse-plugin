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

public class SnykWizardAuthenticate extends WizardPage implements Listener {
  private Text endpoint;
  private Button unknownCerts;

  private Button authenticate;

  public SnykWizardAuthenticate() {
    super("Snyk Wizard");
    setTitle("Authenticate");
    setDescription("We must now authenticate with Snyk.");
  }

  @Override
  public void createControl(Composite parent) {
    Composite composite = new Composite(parent, SWT.NONE);
    GridLayout gl = new GridLayout();
    GridData gd = new GridData(GridData.FILL_HORIZONTAL);

    int ncol = 2;
    gl.numColumns = ncol;
    composite.setLayout(gl);

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

    // Authenticate
    Label authLabel = new Label(composite, SWT.NONE);
    authLabel.setText("Authenticate with Snyk API:");
    authLabel.setToolTipText("This will open a new webpage to authenticate with Snyk.");

    authenticate = new Button(composite, SWT.PUSH);
    authenticate.setText("Authenticate");
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
    endpoint.setText(Preferences.getInstance().getEndpoint());
    unknownCerts.setSelection(Preferences.getInstance().getBooleanPref(Preferences.INSECURE_KEY));
  }
}
