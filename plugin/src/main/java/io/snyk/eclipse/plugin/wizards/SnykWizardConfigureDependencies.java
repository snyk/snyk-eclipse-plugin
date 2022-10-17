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
  private Composite container;


  public SnykWizardConfigureDependencies() {
      super("Snyk Wizard");
      setTitle("Configure Snyk Dependencies");
      setDescription("Set the Snyk Language Server and CLI path, or let Snyk management automatically.");
  }
  
  @Override
  public void createControl(Composite parent) {
    container = new Composite(parent, SWT.NONE);
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    container.setLayout(layout);
    
    new Label(container, SWT.NONE).setText("Update and install Snyk binaries automatically: If disabled, no updates are downloaded and updates must be performed manually. Please make sure that the locations for Language Server and CLI point to an existent, current binary.");
    manageDependenciesEnabled = new Button(container, SWT.CHECK);
    manageDependenciesEnabled.setSelection(false);
    
    new Label(container, SWT.NONE).setText("Snyk Language Server: Specify the location of your language server binary.");
    languageServer = new Text(container, SWT.NONE);
    
    new Label(container, SWT.NONE).setText("Snyk CLI: Specify the location of the Snyk CLI .");
    cli = new Text(container, SWT.NONE);
    
    GridData gd = new GridData(GridData.FILL_HORIZONTAL);
    languageServer.setLayoutData(gd);
    cli.setLayoutData(gd);
    manageDependenciesEnabled.setLayoutData(gd);

    // required to avoid an error in the system
    setControl(container);
    setPageComplete(false);
  }

  public void handleEvent(Event e) {
    getWizard().getContainer().updateButtons();
  }
  
  public IWizardPage getNextPage() {
    updateModel();
    
    return ((SnykWizard)getWizard()).configureAPI;
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
