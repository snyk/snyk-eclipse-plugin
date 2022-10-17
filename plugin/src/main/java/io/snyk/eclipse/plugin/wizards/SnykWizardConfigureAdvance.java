package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

public class SnykWizardConfigureAdvance extends WizardPage implements Listener {
  private Text organization;
  private Text additionalParameters;
  private Text additionalEnvironment;
  private Composite container;


  public SnykWizardConfigureAdvance() {
      super("Snyk Wizard");
      setTitle("Configure advanced options");
      setDescription("Set advanced options for Snyk.");
  }

  @Override
  public void createControl(Composite parent) {
    // TODO Auto-generated method stub
    container = new Composite(parent, SWT.NONE);
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    container.setLayout(layout);
    
    new Label(container, SWT.NONE).setText("Organization: Specify the Snyk Organization to use for scanning.");
    organization = new Text(container, SWT.NONE);
    
    new Label(container, SWT.NONE).setText("Additional Parameters: Specify additional parameters to pass to the CLI (for example, --all-projects or -d).");
    additionalParameters = new Text(container, SWT.NONE);
    
    new Label(container, SWT.NONE).setText("Additional Environment: Add environment variables to Language Server, multiple can be separated by ;. (e.g. JAVA_HOME=/usr/local/bin;GOPATH=/usr/local/bin).");
    additionalEnvironment = new Text(container, SWT.NONE);
    
    GridData gd = new GridData(GridData.FILL_HORIZONTAL);
    organization.setLayoutData(gd);
    additionalParameters.setLayoutData(gd);
    additionalEnvironment.setLayoutData(gd);
    
    // required to avoid an error in the system
    setControl(container);
    setPageComplete(false);
  }

  public void handleEvent(Event e) {
    getWizard().getContainer().updateButtons();
  }
  
  public boolean isPageComplete() {
    updateModel();
    return true;
  }
  
  private void updateModel() {
    SnykWizard wizard = (SnykWizard)getWizard();
    SnykWizardModel model = wizard.model;
    
    if (organization.getText().length() > 0) {
      model.organization= organization.getText();
    }
    
    if (additionalParameters.getText().length() > 0) {
      model.additionalParameters= additionalParameters.getText();
    }
    
    if (additionalEnvironment.getText().length() > 0) {
      model.additionalEnvironment= additionalEnvironment.getText();
    }

    wizard.configureAdvanceCompleted = true;
  }
}
