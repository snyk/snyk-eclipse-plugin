package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Event;

public class SnykWizardConfigureDependencies extends WizardPage implements Listener {
  private Text languageServer;
  private Text cli;
  private Button manageDependenciesEnabled;


  public SnykWizardConfigureDependencies() {
      super("Snyk Wizard");
      setTitle("Configure Snyk Dependencies");
      setDescription("Set the Snyk Language Server and CLI path, or let Snyk management automatically.");
  }
  
  @Override
  public void createControl(Composite parent) {
    Composite composite = new Composite(parent, SWT.NONE);
    GridLayout gl = new GridLayout();
    GridData gd = new GridData(GridData.FILL_HORIZONTAL);
    
    int ncol = 2;
    gl.numColumns = ncol;
    composite.setLayout(gl);
    
    Label manageDependenciesLabel = new Label(composite, SWT.NONE);
    manageDependenciesLabel.setText("Update and install Snyk binaries automatically:");
    manageDependenciesLabel.setToolTipText("If disabled, no updates are downloaded and updates must be performed manually. Please make sure that the locations for Language Server and CLI point to an existent, current binary.");

    manageDependenciesEnabled = new Button(composite, SWT.CHECK);
    manageDependenciesEnabled.setLayoutData(gd);
    manageDependenciesEnabled.setSelection(true);    
    
    Label languageServerLabel = new Label(composite, SWT.NONE);
    languageServerLabel.setText("Snyk Language Server:");
    languageServerLabel.setToolTipText("Specify the location of your language server binary.");

    languageServer = new Text(composite, SWT.BORDER);
    languageServer.setLayoutData(gd);
    
    Label cliLabel = new Label(composite, SWT.NONE);
    cliLabel.setText("Snyk CLI:");
    cliLabel.setToolTipText("Specify the location of the Snyk CLI.");

    cli = new Text(composite, SWT.BORDER);
    cli.setLayoutData(gd);
    
    // required to avoid an error in the system
    setControl(composite);
    setPageComplete(false);
  }

  public void handleEvent(Event e) {
    getWizard().getContainer().updateButtons();
  }
  
  public IWizardPage getNextPage() {
    updateModel();
    
    return ((SnykWizard)getWizard()).configureAPI;
  }
  
  public boolean canFlipToNextPage() {
    return true;
  }
  
  private void updateModel() {
    SnykWizard wizard = (SnykWizard)getWizard();
    SnykWizardModel model = wizard.model;
    
    if (languageServer.getText().length() > 0) {
      model.languageServer = languageServer.getText();
    }
    
    if (cli.getText().length() > 0) {
      model.cli = cli.getText();
    }
    
    if (manageDependenciesEnabled.isEnabled()) {
      model.manageDependenciesEnabled = manageDependenciesEnabled.isEnabled();
    }
  }
}
