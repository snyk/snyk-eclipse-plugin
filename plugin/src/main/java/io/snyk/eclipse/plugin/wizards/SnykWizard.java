package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class SnykWizard extends Wizard implements INewWizard {
  protected SnykWizardConfigureAPI configureAPI = new SnykWizardConfigureAPI();
  protected SnykWizardConfigureProducts configureProducts = new SnykWizardConfigureProducts();
  protected SnykWizardConfigureDependencies configureDependencies = new SnykWizardConfigureDependencies();
  protected SnykWizardConfigureAdvance configureAdvanced= new SnykWizardConfigureAdvance();

  public SnykWizard() {
    super();
    setNeedsProgressMonitor(true);
  }
  
  @Override
  public String getWindowTitle() {
    return "Snyk Wizard";
  }
  
  @Override
  public void addPages() {
    addPage(configureDependencies);
    addPage(configureAPI);
    addPage(configureProducts);
    addPage(configureAdvanced);
  }

  @Override
  public void init(IWorkbench workbench, IStructuredSelection selection) {
    // TODO Auto-generated method stub

  }

  @Override
  public boolean performFinish() {
    
    return true;
  }

}
