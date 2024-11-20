package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.utils.SnykMessageDialog;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class StopScanHandler extends AbstractHandler {

	public StopScanHandler() {
		super();

	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		SnykExtendedLanguageClient.getInstance().cancelAllProgresses();
		return null;
	}
}