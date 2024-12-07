package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.ToggleProductFilters;

public class EnableAllProductHandler extends BaseHandler implements IElementUpdater {

	public EnableAllProductHandler() {
		super();

		iconEnabled = SnykIcons.ENABLED;
		iconDisabled = SnykIcons.DISABLED;
		preferenceKey = null;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		new ToggleProductFilters(TreeFilterManager.getInstance(), Preferences.getInstance()).applyFilter();

		// Lastly update the UI.
		refreshCommands();

		return null;
	}

	private void refreshCommands() {
		ICommandService commandService = PlatformUI.getWorkbench().getService(ICommandService.class);
		if (commandService != null) {
			commandService.refreshElements("io.snyk.eclipse.plugin.commands.enableOSS", null);
			commandService.refreshElements("io.snyk.eclipse.plugin.commands.enableCodeSecurity", null);
			commandService.refreshElements("io.snyk.eclipse.plugin.commands.enableCodeQuality", null);
			commandService.refreshElements("io.snyk.eclipse.plugin.commands.enableIAC", null);
			commandService.refreshElements("io.snyk.eclipse.plugin.command.snykShowAllProducts", null);
		}
	}
}