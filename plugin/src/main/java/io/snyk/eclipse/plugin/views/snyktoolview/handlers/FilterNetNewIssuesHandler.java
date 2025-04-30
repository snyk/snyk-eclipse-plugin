package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import java.util.concurrent.CompletableFuture;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

import io.snyk.eclipse.plugin.SnykStartup;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class FilterNetNewIssuesHandler extends BaseHandler implements IElementUpdater {

	public FilterNetNewIssuesHandler() {
		super();
		preferenceKey = Preferences.ENABLE_DELTA;

	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);

		boolean booleanPref = Preferences.getInstance().getBooleanPref(this.preferenceKey);

		if (booleanPref) {
			SnykStartup.getView().enableDelta();
		} else {
			SnykStartup.getView().disableDelta();
		}
		
		CompletableFuture.runAsync(() -> {
			// Update the Snyk Language Server configuration.
			SnykExtendedLanguageClient lc = SnykExtendedLanguageClient.getInstance();
			lc.updateConfiguration();
			if (Preferences.getInstance().getBooleanPref(Preferences.SCANNING_MODE_AUTOMATIC)) {
				lc.triggerScan(null);
			}
		});

		return null;
	}

}
