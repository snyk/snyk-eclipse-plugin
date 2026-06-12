package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class StopScanHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		SnykExtendedLanguageClient.getInstance().getProgressManager().cancelAll();
		return null;
	}
}
