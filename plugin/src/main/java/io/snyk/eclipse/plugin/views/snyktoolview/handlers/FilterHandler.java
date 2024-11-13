package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;

public class FilterHandler extends AbstractHandler implements IElementUpdater {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		String commandId = event.getCommand().getId();

		switch (commandId) {
		case "io.snyk.eclipse.plugin.commands.snykFilterCritical":
			toggleFilter(Preferences.FILTER_CRITICAL);
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

	@Override
	public void updateElement(UIElement element, @SuppressWarnings("rawtypes") Map map) {
		// TODO update the filter buttons when state changed.
	}

	private void toggleFilter(String filter) {
		String preference = getPreference(filter);

		// Toggle the value, if it was true, it should be set to false
		if (Boolean.parseBoolean(preference)) {
			Preferences.getInstance().store(filter, "false");

		} else {
			Preferences.getInstance().store(filter, "true");

		}
	}

	private String getPreference(String preference) {
		return Preferences.getInstance().getPref(preference);
	}

}