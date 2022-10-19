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
  protected SnykWizardSummary summary;
  
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
    configureDependencies = new SnykWizardConfigureDependencies();
    addPage(configureDependencies);
    
    configureAPI= new SnykWizardConfigureAPI(); 
    addPage(configureAPI);
    
    configureProducts = new SnykWizardConfigureProducts();
    addPage(configureProducts);
    
    configureAdvance = new SnykWizardConfigureAdvance();
    addPage(configureAdvance);
    
    summary = new SnykWizardSummary();
    addPage(summary);
  }

  public void init(IWorkbench workbench, IStructuredSelection selection) {
    // TODO
    this.workbench = workbench;
    this.selection = selection;
  }
  
  public boolean canFinish() {
    if (this.getContainer().getCurrentPage() == summary) {
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
