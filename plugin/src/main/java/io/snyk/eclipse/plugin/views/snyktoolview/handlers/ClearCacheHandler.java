package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class ClearCacheHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		SnykExtendedLanguageClient.getInstance().clearCache();
		ScanState.getInstance().clearAllScanStates();
		return null;
	}

}