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

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.languageserver.LsConfigurationUpdater;

public class BaseHandler extends AbstractHandler implements IElementUpdater {

	protected ImageDescriptor iconEnabled = null;
	protected ImageDescriptor iconDisabled = null;
	protected String preferenceKey = null;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String commandId = event.getCommand().getId();

		// Update the application state for the preference.
		Preferences.getInstance().store(preferenceKey,
				Boolean.valueOf(!Preferences.getInstance().getBooleanPref(preferenceKey)).toString());

		// Update the Snyk Language Server configuration.
		new LsConfigurationUpdater().configurationChanged();

		// Lastly update the UI.
		ICommandService commandService = PlatformUI.getWorkbench().getService(ICommandService.class);
		if (commandService != null) {
			commandService.refreshElements(commandId, null);
		}

		return null;
	}

	@Override
	public void updateElement(UIElement element, @SuppressWarnings("rawtypes") Map map) {
		boolean enabled = Preferences.getInstance().getBooleanPref(preferenceKey);
		ImageDescriptor icon = enabled ? iconEnabled : iconDisabled;
		element.setIcon(icon);
	}

}