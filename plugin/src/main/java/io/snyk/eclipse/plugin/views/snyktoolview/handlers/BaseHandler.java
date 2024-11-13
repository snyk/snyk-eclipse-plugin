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

public class BaseHandler extends AbstractHandler implements IElementUpdater {

	//TODO should we replace the filter button with a filter applied button icon?
	protected ImageDescriptor iconEnabled = null;
	protected ImageDescriptor iconDisabled = null;
	protected String preferenceKey = null;

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

		String preference = Preferences.getInstance().getPref(preferenceKey);

		// Toggle the value, if it was true, it should be set to false
		if (Boolean.parseBoolean(preference)) {
			element.setIcon(iconDisabled);
			Preferences.getInstance().store(preferenceKey, "false");

		} else {

			element.setIcon(iconEnabled);
			Preferences.getInstance().store(preferenceKey, "true");

		}

	}
}