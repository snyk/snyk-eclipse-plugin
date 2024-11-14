package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.utils.SnykMessageDialog;

public class ClearCacheHandler extends AbstractHandler {

	public ClearCacheHandler() {
		super();

	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String commandId = event.getCommand().getId();

		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		SnykMessageDialog.showOkDialog(shell, commandId);

		return null;
	}

}