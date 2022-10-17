package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class SnykWizard extends Wizard implements INewWizard {
  protected SnykWizardConfigureAPI configureAPI;
  protected SnykWizardConfigureProducts configureProducts;
  protected SnykWizardConfigureDependencies configureDependencies;
  protected SnykWizardConfigureAdvance configureAdvance;
  
  protected SnykWizardModel model;
  
  protected IWorkbench workbench;
  protected IStructuredSelection selection;

  public SnykWizard() {
    super();
    // TODO import preferences so wizard can update them?
    model = new SnykWizardModel();
    setNeedsProgressMonitor(true);
  }
  
  @Override
  public String getWindowTitle() {
    return "Snyk Wizard";
  }
  
  @Override
  public void addPages() {
    configureDependencies = new SnykWizardConfigureDependencies();
    addPage(configureDependencies);
    
    configureAPI= new SnykWizardConfigureAPI(workbench); 
    addPage(configureAPI);
    
    configureProducts = new SnykWizardConfigureProducts();
    addPage(configureProducts);
    
    configureAdvance = new SnykWizardConfigureAdvance();
    addPage(configureAdvance);
  }

  public void init(IWorkbench workbench, IStructuredSelection selection) {
    // TODO
    this.workbench = workbench;
    this.selection = selection;
  }
  
  public boolean canFinish() {
    if (this.getContainer().getCurrentPage() == configureAdvance) {
      return true;
    }
    return false;
  }

  public boolean performFinish() {
    return true;
  }
}
