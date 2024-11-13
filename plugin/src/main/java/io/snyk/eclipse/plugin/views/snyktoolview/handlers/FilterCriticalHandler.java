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

public class FilterCriticalHandler extends AbstractHandler implements IElementUpdater {

	//TODO should we replace the filter button with a filter applied button icon?
	protected static ImageDescriptor FILTER_ENABLE = Activator.getImageDescriptor("/icons/severity-critical.png");
	protected static ImageDescriptor FILTER_DISABLE = Activator.getImageDescriptor("/icons/oss_disabled.png");
	protected static String FILTER = Preferences.FILTER_CRITICAL;

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

		String preference = Preferences.getInstance().getPref(FILTER);

		// Toggle the value, if it was true, it should be set to false
		if (Boolean.parseBoolean(preference)) {
//			element.setIcon(FILTER_DISABLE);
			Preferences.getInstance().store(FILTER, "false");

		} else {

//			element.setIcon(FILTER_ENABLE);
			Preferences.getInstance().store(FILTER, "true");

		}

	}
}