package io.snyk.eclipse.plugin.views;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;

import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class MenuHandler extends AbstractHandler {

  public Object execute(ExecutionEvent event) throws ExecutionException {
    IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked(event);
    SnykExtendedLanguageClient.getInstance().triggerScan(window);
    return null;
  }
}
