package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;

import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

public class SnykWizardAuthenticatePage extends WizardPage implements Listener {
  private Text endpoint;
  private Button unknownCerts;

  public SnykWizardAuthenticatePage() {
    super("Snyk Wizard");
    setTitle("Authenticate");
    setDescription("Review the endpoint configuration, clicking 'Finish' will authenticate with Snyk; this will open a new browser window.");
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
    endpointLabel.setText("Endpoint:");

    endpoint = new Text(composite, SWT.BORDER | SWT.READ_ONLY);
    endpoint.setLayoutData(gd);
    
    createLine(composite, ncol);

    Label unknownCertsLabel = new Label(composite, SWT.NONE);
    unknownCertsLabel.setText("Allow unknown certificate authorities:");

    unknownCerts = new Button(composite, SWT.CHECK);
    unknownCerts.setLayoutData(gd);

    // required to avoid an error in the system
    setControl(composite);
    setPageComplete(false);
  }

  public void handleEvent(Event e) {
    getWizard().getContainer().updateButtons();
  }
  
  public boolean isPageComplete() {
    return true;
  }

  void onEnterPage() {
    endpoint.setText(Preferences.getInstance().getEndpoint());
    unknownCerts.setSelection(Preferences.getInstance().getBooleanPref(Preferences.INSECURE_KEY));
  }
  
  private void createLine(Composite parent, int ncol) {
    Label line = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.BOLD);
    GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
    gridData.horizontalSpan = ncol;
    line.setLayoutData(gridData);
  }
}
