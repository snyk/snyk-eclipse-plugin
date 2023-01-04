package io.snyk.eclipse.plugin.views;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;

import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class ScanWorkspaceMenuHandler extends AbstractHandler {

  public Object execute(ExecutionEvent event) throws ExecutionException {
    SnykExtendedLanguageClient.getInstance().triggerScan(null);
    return null;
  }
}
