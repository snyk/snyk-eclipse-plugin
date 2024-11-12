package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import io.snyk.eclipse.plugin.utils.SnykMessageDialog;

public class HideIgnoredHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		// TODO Auto-generated method stub
		String commandId = event.getCommand().getId();

		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		SnykMessageDialog.showOkDialog(shell, commandId);

		return null;
	}

}
