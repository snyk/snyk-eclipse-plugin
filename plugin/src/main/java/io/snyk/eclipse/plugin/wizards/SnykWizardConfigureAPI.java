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

	public SnykWizardConfigureAPI(IWorkbench workbench) {
		super("Snyk Wizard");
		setTitle("Configure Snyk API");
		setDescription("Configure your Snyk API.");
		
		this.workbench = workbench;
	}
	
  @Override
  public void createControl(Composite parent) {
      Composite composite = new Composite(parent, SWT.NONE);
      GridLayout gl = new GridLayout();
      GridData gd = new GridData(GridData.FILL_HORIZONTAL);
      
      int ncol = 2;
      gl.numColumns = ncol;
      composite.setLayout(gl);
      
      Label pathLabel = new Label(composite, SWT.NONE);
      pathLabel.setText("Path:");
      pathLabel.setToolTipText("Specify your additions to the path to find needed third party tools such as Gradle or Maven.");

      path = new Text(composite, SWT.BORDER);
      path.setLayoutData(gd);
      
      Label endpointLabel = new Label(composite, SWT.NONE);
      endpointLabel.setText("Custom Endpoint:");
      endpointLabel.setToolTipText("Specify the custom endpoint for Single Tenant setups instead of https://app.snyk.io.");

      endpoint = new Text(composite, SWT.BORDER);
      endpoint.setLayoutData(gd);
      
      Label unknownCertsLabel = new Label(composite, SWT.NONE);
      unknownCertsLabel.setText("Allow unknown certificate authorities:");
      unknownCertsLabel.setToolTipText("Disable certificate checks for SSL connections.");

      unknownCerts = new Button(composite, SWT.CHECK);
      unknownCerts.setSelection(false);
      unknownCerts.setLayoutData(gd);
      
      new Label(composite, SWT.NONE);
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
      // TODO handle authentication event      
    }
 
    getWizard().getContainer().updateButtons();
  }
  
  public boolean canFlipToNextPage() {
    return true;
  }
  
  public IWizardPage getNextPage() {
    updateModel();
    
    // TODO get next page if auth successful
    return ((SnykWizard)getWizard()).configureProducts;
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
