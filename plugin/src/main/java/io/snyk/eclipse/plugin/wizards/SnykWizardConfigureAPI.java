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

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.LsConfigurationUpdater;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

public class SnykWizardConfigureAPI extends WizardPage implements Listener {
	private Text endpoint;
	private Button unknownCerts;
	private Button authenticate;

	public SnykWizardConfigureAPI() {
		super("Snyk Wizard");
		setTitle("Configure Snyk API");
		setDescription("Configure your Snyk API.");
		
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
      updatePreferences();
      SnykExtendedLanguageClient.getInstance().triggerAuthentication();

      MessageDialog.openInformation(this.getShell(), "Authenticating", "Authenticating with Snyk...");
    }
 
    getWizard().getContainer().updateButtons();
  }
  
  public boolean canFlipToNextPage() {
    return true;
  }
  
  public IWizardPage getNextPage() {
    updatePreferences();
    return ((SnykWizard)getWizard()).configureProducts;
  }
  
  private void updatePreferences() {
    Preferences.getInstance().store(Preferences.ENDPOINT_KEY, endpoint.getText());
    Preferences.getInstance().store(Preferences.INSECURE_KEY, Boolean.toString(unknownCerts.getSelection()));
    
    new LsConfigurationUpdater().configurationChanged();
  }
}
