package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class EnableIacProductHandler extends AbstractHandler implements IElementUpdater {

	protected static ImageDescriptor IMAGE_ENABLE = Activator.getImageDescriptor("/icons/iac.png");
	protected static ImageDescriptor IMAGE_DISABLE = Activator.getImageDescriptor("/icons/iac_disabled.png");

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String commandId = event.getCommand().getId();

		ICommandService commandService = PlatformUI.getWorkbench().getService(ICommandService.class);
		if (commandService != null) {
			commandService.refreshElements(commandId, null);
		}

		return null;
	}

	@Override
	public void updateElement(UIElement element, @SuppressWarnings("rawtypes") Map map) {

		String enableScanPreference = Preferences.getInstance().getPref(Preferences.ACTIVATE_SNYK_IAC);

		// Toggle the value, if it was true, it should be set to false
		if (Boolean.parseBoolean(enableScanPreference)) {
			element.setIcon(IMAGE_DISABLE);
			Preferences.getInstance().store(Preferences.ACTIVATE_SNYK_IAC, "false");

		} else {

			element.setIcon(IMAGE_ENABLE);
			Preferences.getInstance().store(Preferences.ACTIVATE_SNYK_IAC, "true");

		}
	}
}