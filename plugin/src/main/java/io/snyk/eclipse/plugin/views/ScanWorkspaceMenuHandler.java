package io.snyk.eclipse.plugin.views;

import java.util.concurrent.CompletableFuture;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class ScanWorkspaceMenuHandler extends AbstractHandler {

  public Object execute(ExecutionEvent event) throws ExecutionException {
	  CompletableFuture.runAsync(() -> {
			SnykExtendedLanguageClient.getInstance().triggerScan(null);
		});
    return null;
  }
}
