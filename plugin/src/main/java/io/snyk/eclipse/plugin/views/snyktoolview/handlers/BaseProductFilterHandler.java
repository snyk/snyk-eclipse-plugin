package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class BaseProductFilterHandler extends BaseHandler {

	public BaseProductFilterHandler(@SuppressWarnings("PMD.UnusedFormalParameter") String filterName) {
		super();
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		var returnValue = super.execute(event);
		SnykExtendedLanguageClient.getInstance().updateConfiguration();
		return returnValue;
	}
}
