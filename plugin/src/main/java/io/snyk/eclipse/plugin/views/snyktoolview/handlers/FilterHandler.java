package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.utils.SnykMessageDialog;

public class FilterHandler extends AbstractHandler {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		String commandId = event.getCommand().getId();

		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		SnykMessageDialog.showOkDialog(shell, commandId);

		switch (commandId) {
		case "io.snyk.eclipse.plugin.commands.snykFilterCritical":
			// Implement behavior for command1
			break;
		case "io.snyk.eclipse.plugin.commands.snykFilterHigh":
			// Implement behavior for command2
			break;
		case "io.snyk.eclipse.plugin.commands.snykFilterMedium":
			// Implement behavior for command3
			break;
		case "io.snyk.eclipse.plugin.commands.snykFilterLow":
			// Implement behavior for command4
			break;
		case "io.snyk.eclipse.plugin.commands.snykFilterDelta":
			// Implement behavior for command5
			break;
		case "io.snyk.eclipse.plugin.commands.filter":
			// Implement behavior for command6
			break;
		}

		return null;
	}
}