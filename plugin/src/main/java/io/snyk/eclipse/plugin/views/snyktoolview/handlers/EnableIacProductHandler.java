package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.utils.SnykMessageDialog;

public class EnableIacProductHandler extends AbstractHandler implements IElementUpdater {

	protected static ImageDescriptor IMAGE_IAC_ENABLE = Activator.getImageDescriptor("/icons/iac.png");
	protected static ImageDescriptor IMAGE_IAC_DISABLE = Activator.getImageDescriptor("/icons/iac_disabled.png");

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		String commandId = event.getCommand().getId();

		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		SnykMessageDialog.showOkDialog(shell, commandId);

		ICommandService commandService = PlatformUI.getWorkbench().getService(ICommandService.class);
		if (commandService != null) {
			commandService.refreshElements(commandId, null);
		}

		return null;
	}

	@Override
	public void updateElement(UIElement element, @SuppressWarnings("rawtypes") Map map) {
		boolean condition = true;

		// TODO check the configuration is we should enable or disable the product scan.

//		if (condition) {
//			element.setIcon(IMAGE_OSS_ENABLE);
//		} else {
//			element.setIcon(IMAGE_OSS_DISABLE);
//		}
	}
}