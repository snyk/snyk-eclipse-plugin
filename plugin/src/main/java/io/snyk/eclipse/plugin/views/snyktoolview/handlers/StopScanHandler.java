package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.snyktoolview.ISnykToolView;
import io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView;
import io.snyk.languageserver.ScanState;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class StopScanHandler extends AbstractHandler {

	private ISnykToolView toolView;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		SnykExtendedLanguageClient.getInstance().getProgressManager().cancelAll();
		ScanState.getInstance().clearAllScanStates();
		PlatformUI.getWorkbench().getDisplay().asyncExec(() -> {
			IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			try {
				toolView = (ISnykToolView) activePage.showView(SnykToolView.ID);
				toolView.clearTree();
				toolView.refreshTree();
			} catch (PartInitException e) {
				SnykLogger.logError(e);
			}
		});

		return null;
	}
}