package io.snyk.eclipse.plugin.wizards;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.handlers.HandlerUtil;

public class SnykWizardMenuHandler extends AbstractHandler {
  IWorkbenchPart part;
  ISelection selection;
  
  public void setActivePart(IAction action, IWorkbenchPart part) {
    this.part = part;
  }
  
  public Object execute(ExecutionEvent event) throws ExecutionException {
    Shell activeShell = HandlerUtil.getActiveShell(event);

    SnykWizard wizard = new SnykWizard();

    WizardDialog dialog = new WizardDialog(activeShell, wizard);

    dialog.open();

    return null;
  }
  
  public void selectionChanged(IAction action, ISelection selection) {
    this.selection = selection;
  }
}
