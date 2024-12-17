package io.snyk.eclipse.plugin.views.snyktoolview.handlers;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_QUALITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_OPEN_SOURCE;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_CODE_SECURITY;
import static io.snyk.eclipse.plugin.preferences.Preferences.ACTIVATE_SNYK_IAC;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_CRITICAL;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_HIGH;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_LOW;
import static io.snyk.eclipse.plugin.preferences.Preferences.FILTER_SHOW_MEDIUM;

import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykIcons;
import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class BaseHandler extends AbstractHandler implements IElementUpdater, IHandlerCommands {
	protected ImageDescriptor iconEnabled = null;
	protected ImageDescriptor iconDisabled = null;
	protected String preferenceKey = null;
	
	public BaseHandler() {
		iconEnabled = SnykIcons.ENABLED;
		iconDisabled = SnykIcons.DISABLED;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String commandId = event.getCommand().getId();

		// Update the application state for the preference.
		Preferences.getInstance().store(preferenceKey,
				Boolean.toString(!Preferences.getInstance().getBooleanPref(preferenceKey)));

		// Update the Snyk Language Server configuration.
		SnykExtendedLanguageClient.getInstance().updateConfiguration();

		updateUIForCommand(commandId);

		return null;
	}

	protected void updateUIForCommand(String commandId) {
		// Lastly update the UI.
		ICommandService commandService = PlatformUI.getWorkbench().getService(ICommandService.class);
		if (commandService != null && commandId != null) {
			commandService.refreshElements(commandId, null);
		}
	}

	@Override
	public void updateElement(UIElement element, @SuppressWarnings("rawtypes") Map map) {
		boolean enabled = Preferences.getInstance().getBooleanPref(preferenceKey);
		ImageDescriptor icon = enabled ? iconEnabled : iconDisabled;
		element.setIcon(icon);
	}
	

	@Override
	public String getCommandByPref(String pref) {
		switch(pref) {
		case ACTIVATE_SNYK_OPEN_SOURCE:
			return ENABLE_OSS;
		case ACTIVATE_SNYK_CODE_QUALITY:
			return ENABLE_CODE_QUALITY;
		case ACTIVATE_SNYK_CODE_SECURITY:
			return ENABLE_CODE_SECURITY;
		case ACTIVATE_SNYK_IAC:
			return ENABLE_IAC;
		case FILTER_SHOW_CRITICAL:
			return SHOW_CRITICAL;
		case FILTER_SHOW_HIGH:
			return SHOW_HIGH;
		case FILTER_SHOW_MEDIUM:
			return SHOW_MEDIUM;
		case FILTER_SHOW_LOW:
			return SHOW_LOW;
		case Preferences.ENABLE_DELTA:
			return ENABLE_DELTA;
		case FILTER_IGNORES_SHOW_IGNORED_ISSUES:
			return IGNORES_SHOW_IGNORED;
		case FILTER_IGNORES_SHOW_OPEN_ISSUES:
			return IGNORES_SHOW_OPEN;
		default:
			return null;
		}
	}

	protected void updateMultiplePrefs(List<String> prefs) {
		// if all are enabled, disable all
		// if one is disabled, enable all
		Preferences preferences = Preferences.getInstance();
		boolean allEnabled = true;
		for (String p : prefs) {
			allEnabled &= preferences.getBooleanPref(p);
		}
	
		for (String p : prefs) {			
			preferences.store(p, Boolean.toString(!allEnabled));
			updateUIForCommand(getCommandByPref(p));
		}
		
		TreeFilterManager.getInstance().reset();
	}

}