package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Button;

public class SnykWizardConfigureAPI extends WizardPage {
	private Text path;
	private Text endpoint;
	private Button unknownCerts;
	private Composite container;

	public SnykWizardConfigureAPI() {
		super("Snyk Wizard");
		setTitle("Configure Snyk API");
		setDescription("Configure your Snyk API.");
	}
	
  @Override
  public void createControl(Composite parent) {
      container = new Composite(parent, SWT.NONE);
      GridLayout layout = new GridLayout();
      layout.numColumns = 2;
      container.setLayout(layout);
      
      new Label(container, SWT.NONE).setText("Path: Specify your additions to the path to find needed third party tools such as Gradle or Maven.");
      path = new Text(container, SWT.NONE);
      this.setPath(path);
      
      new Label(container, SWT.NONE).setText("Custom Endpoint: Specify the custom endpoint for Single Tenant setups instead of https://app.snyk.io.");
      endpoint = new Text(container, SWT.NONE);
      this.setEndpoint(endpoint);
      
      new Label(container, SWT.NONE).setText("Allow unknown certificate authorities: Disable certificate checks for SSL connections.");
      unknownCerts = new Button(container, SWT.CHECK);
      unknownCerts.setSelection(false);
      this.setUnknownCerts(unknownCerts);
      
      new Label(container, SWT.NONE).setText("Authenticate.");
      new Button(container, SWT.BUTTON1);

      GridData gd = new GridData(GridData.FILL_HORIZONTAL);
      path.setLayoutData(gd);
      endpoint.setLayoutData(gd);
      unknownCerts.setLayoutData(gd);

      // required to avoid an error in the system
      setControl(container);
      setPageComplete(false);
  }
  
  private void setPath(Text path) {
    // TODO set preferences
  }
  
  private void setEndpoint(Text endpoint) {
    // TODO set preferences
  }
  
  private void setUnknownCerts(Button unknownCerts) {
    // TODO set preferences
  }
  
  public String getPath() {
    return path.getText();
  }

  public String getEndpoint() {
      return endpoint.getText();
  }
  
  public Boolean getUnknownCerts() {
      return unknownCerts.getSelection();
  }
}
