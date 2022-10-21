package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class SnykWizard extends Wizard implements INewWizard {
  protected SnykWizardConfigureAPI configureAPI;
  protected SnykWizardAuthenticate authenticate;
  
  protected SnykWizardModel model;
   
  protected IWorkbench workbench;
  protected IStructuredSelection selection;

  public SnykWizard() {
    super();
    model = new SnykWizardModel();
    setNeedsProgressMonitor(true);
  }
  
  @Override
  public String getWindowTitle() {
    return "Snyk Wizard";
  }
  
  @Override
  public void addPages() {
    configureAPI= new SnykWizardConfigureAPI(); 
    addPage(configureAPI);
    
    authenticate = new SnykWizardAuthenticate(); 
    addPage(authenticate);
  }

  public void init(IWorkbench workbench, IStructuredSelection selection) {
    this.workbench = workbench;
    this.selection = selection;
  }
  
  public boolean canFinish() {
    boolean isOnFinalPage = this.getContainer().getCurrentPage() == authenticate;
    
    if (isOnFinalPage) {
      return true;
    }
    return false;
  }
  
  public boolean performCancel() {
    model.resetPreferences();
    return true;
  }

  public boolean performFinish() {
    return true;
  }
}
