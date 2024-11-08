package io.snyk.eclipse.plugin.views.snyktoolview.providers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;

import io.snyk.eclipse.plugin.utils.SnykMessageDialog;

public class EnableProductHandler extends AbstractHandler {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		String commandId = event.getCommand().getId();

		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		SnykMessageDialog.showOkDialog(shell, commandId);

		switch (commandId) {
		case "io.snyk.eclipse.plugin.commands.enableOSS":
			// Implement behavior for command1
			break;
		case "io.snyk.eclipse.plugin.commands.enableCode":
			// Implement behavior for command2
			break;
		case "io.snyk.eclipse.plugin.commands.enableIac":
			// Implement behavior for command3
			break;
		}

		return null;
	}
}