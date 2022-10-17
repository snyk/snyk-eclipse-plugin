package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

public class SnykWizardConfigureAPI extends WizardPage implements Listener {
	IWorkbench workbench;
  
    private Text path;
	private Text endpoint;
	private Button unknownCerts;
	private Button authenticate;
	private Composite container;

	public SnykWizardConfigureAPI(IWorkbench workbench) {
		super("Snyk Wizard");
		setTitle("Configure Snyk API");
		setDescription("Configure your Snyk API.");
		
		this.workbench = workbench;
	}
	
  @Override
  public void createControl(Composite parent) {
      container = new Composite(parent, SWT.NONE);
      GridLayout gl = new GridLayout();
      GridData gd = new GridData(GridData.FILL_HORIZONTAL);
      gl.numColumns = 2;
      container.setLayout(gl);
      
      new Label(container, SWT.NONE).setText("Path: Specify your additions to the path to find needed third party tools such as Gradle or Maven.");
      path = new Text(container, SWT.BORDER);
      path.setLayoutData(gd);
      
      new Label(container, SWT.NONE).setText("Custom Endpoint: Specify the custom endpoint for Single Tenant setups instead of https://app.snyk.io.");
      endpoint = new Text(container, SWT.BORDER);
      endpoint.setLayoutData(gd);
      
      new Label(container, SWT.NONE).setText("Allow unknown certificate authorities: Disable certificate checks for SSL connections.");
      unknownCerts = new Button(container, SWT.CHECK);
      unknownCerts.setSelection(false);
      unknownCerts.setLayoutData(gd);
      
      new Label(container, SWT.NONE).setText("Authenticate.");
      authenticate = new Button(container, SWT.CHECK);
      authenticate.setText("Authenticate");
      authenticate.setLayoutData(gd);

      // required to avoid an error in the system
      setControl(container);
      setPageComplete(false);
      this.addListeners();
  }
  
  public void handleEvent(Event e) {
    // TODO handle authentication event
    getWizard().getContainer().updateButtons();
  }
  
  public IWizardPage getNextPage() {
    updateModel();
    
    // TODO get next page if auth successful
    return ((SnykWizard)getWizard()).configureProducts;
  }
  
  private void addListeners() {
    path.addListener(SWT.Selection, this);
    endpoint.addListener(SWT.Selection, this);
  }
  
  private void updateModel() {
    SnykWizard wizard = (SnykWizard)getWizard();
    SnykWizardModel model = wizard.model;
    
    if (path.getText().length() > 0) {
      model.path = path.getText();
    }
    
    if (endpoint.getText().length() > 0) {
      model.endpoint = endpoint.getText();
    }
    
    if (unknownCerts.isEnabled()) {
      model.unknownCerts = unknownCerts.isEnabled();
    }
  }
}
