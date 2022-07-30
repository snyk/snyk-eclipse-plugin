package io.snyk.eclipse.plugin.views;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class MenuHandler extends AbstractHandler {

  public Object execute(ExecutionEvent event) throws ExecutionException {
    SnykExtendedLanguageClient.getInstance().triggerScan();
    
    IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked(event);
    ISelectionService service = window.getSelectionService();
    IStructuredSelection structured = (IStructuredSelection) service.getSelection();

    Object firstElement = structured.getFirstElement();

    if (firstElement instanceof IProject) {
      IProject project = (IProject) firstElement;
      runForProject(project.getName());
    }

    if (firstElement instanceof JavaProject) {
      JavaProject javaproject = (JavaProject) firstElement;
      runForProject(javaproject.getProject().getName());
    }
     
    return null;
  }

  private void runForProject(String projectName) {
    SnykView snykView = SnykStartup.getSnykView();
    if (snykView != null) {
      snykView.testProject(projectName);
    }
  }
}
