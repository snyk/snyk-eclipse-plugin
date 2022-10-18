package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;

import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

public class SnykWizardConfigureAPI extends WizardPage implements Listener {
	IWorkbench workbench;
  
	private Text apiToken;
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
      
      Label tokenLabel = new Label(composite, SWT.NONE);
      tokenLabel.setText("Snyk API Token:");
      tokenLabel.setToolTipText("Set the authentication token from Snyk.");
      
      apiToken = new Text(composite, SWT.BORDER|SWT.PASSWORD);
      apiToken.setText(Preferences.getInstance().getAuthToken());
      apiToken.setLayoutData(gd);
      
      Label pathLabel = new Label(composite, SWT.NONE);
      pathLabel.setText("Path:");
      pathLabel.setToolTipText("Specify your additions to the path to find needed third party tools such as Gradle or Maven.");

      path = new Text(composite, SWT.BORDER);
      path.setText(Preferences.getInstance().getPref(Preferences.PATH_KEY));
      path.setLayoutData(gd);
      
      Label endpointLabel = new Label(composite, SWT.NONE);
      endpointLabel.setText("Custom Endpoint:");
      endpointLabel.setToolTipText("Specify the custom endpoint for Single Tenant setups instead of https://app.snyk.io.");

      endpoint = new Text(composite, SWT.BORDER);
      endpoint.setText(Preferences.getInstance().getEndpoint());
      endpoint.setLayoutData(gd);
      
      Label unknownCertsLabel = new Label(composite, SWT.NONE);
      unknownCertsLabel.setText("Allow unknown certificate authorities:");
      unknownCertsLabel.setToolTipText("Disable certificate checks for SSL connections.");

      unknownCerts = new Button(composite, SWT.CHECK);
      unknownCerts.setSelection(Preferences.getInstance().isInsecure());
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
      updatePreferences();
      MessageDialog.openInformation(this.getShell(), "Authenticating", "Authenticating with Snyk...");
    }
 
    getWizard().getContainer().updateButtons();
  }
  
  public boolean canFlipToNextPage() {
    return true;
  }
  
  public IWizardPage getNextPage() {
    updatePreferences();
    
    // TODO get next page if auth successful
    return ((SnykWizard)getWizard()).configureProducts;
  }
  
  private void updatePreferences() {
    Preferences.getInstance().store(Preferences.AUTH_TOKEN_KEY, apiToken.getText());
    Preferences.getInstance().store(Preferences.PATH_KEY, path.getText());
    Preferences.getInstance().store(Preferences.ENDPOINT_KEY, endpoint.getText());
    Preferences.getInstance().store(Preferences.INSECURE_KEY, Boolean.toString(unknownCerts.getSelection()));
  }
}
