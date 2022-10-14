package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class SnykWizard extends Wizard implements INewWizard {
  protected SnykWizardWelcome welcome = new SnykWizardWelcome();
  protected SnykWizardPageOne one = new SnykWizardPageOne();
  protected SnykWizardPageTwo two = new SnykWizardPageTwo();

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
    addPage(welcome);
    addPage(one);
    addPage(two);
  }

  @Override
  public void init(IWorkbench workbench, IStructuredSelection selection) {
    // TODO Auto-generated method stub

  }

  @Override
  public boolean performFinish() {
    // Print the result to the console
    System.out.println(welcome.getWelcomeMsg());
    System.out.println(one.getText1());
    System.out.println(two.getText1());
    
    return true;
  }

}
