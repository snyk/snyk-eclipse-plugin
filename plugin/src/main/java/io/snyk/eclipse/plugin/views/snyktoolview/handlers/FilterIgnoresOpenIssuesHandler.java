package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.window.ApplicationWindow;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;

public class FilterIgnoresOpenIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterIgnoresOpenIssuesHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		iconDisabled = SnykIcons.DISABLED;
		preferenceKey = Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES;

	}

	@Override
	public boolean isEnabled() {
		// Check the preference state
		boolean isEnabled = InstanceScope.INSTANCE.getNode("io.snyk.eclipse.plugin")
				.getBoolean(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, false);

		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window instanceof ApplicationWindow) {
			MenuManager menuManager = ((ApplicationWindow) window).getMenuBarManager();
			// Now you can use the menuManager
			updateViewMenu(menuManager, isEnabled);
		}

//	    commandService.refreshElements("io.snyk.eclipse.plugin.commands.snykShowOpenIgnored", null);

		return true;
	}

	public void updateViewMenu(MenuManager menuManager, boolean isVisible) {
		if (!isVisible) {
			// Remove the command from the menu if it exists
			menuManager.remove("io.snyk.eclipse.plugin.commands.snykShowOpenIgnored");
		}
		menuManager.update(true); // Refresh the menu
	}

}