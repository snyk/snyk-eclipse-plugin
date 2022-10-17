package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

public class SnykWizardAction implements IObjectActionDelegate {
  IWorkbenchPart part;
  ISelection selection;
  
  public void setActivePart(IAction action, IWorkbenchPart part) {
    this.part = part;
  }
  
  public void run(IAction action) {
    // Instantiates and initialises the wizard
    SnykWizard wizard = new SnykWizard();
    if ((selection instanceof IStructuredSelection) || (selection == null))
    wizard.init(part.getSite().getWorkbenchWindow().getWorkbench(), 
        (IStructuredSelection)selection);
        
    // Instantiates the wizard container with the wizard and opens it
    WizardDialog dialog = new WizardDialog(part.getSite().getShell(), wizard);
    dialog.create();
    dialog.open();
  }
  
  public void selectionChanged(IAction action, ISelection selection) {
    this.selection = selection;
  }
}
