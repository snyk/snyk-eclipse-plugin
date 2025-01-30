package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import io.snyk.eclipse.plugin.views.snyktoolview.TreeFilterManager;
import io.snyk.eclipse.plugin.views.snyktoolview.filters.ProductFilter;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class BaseProductFilterHandler extends BaseHandler {
	private String filterName;

	public BaseProductFilterHandler(String filterName) {
		super();
		this.filterName = filterName;
	}
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		// first we need to execute the super method to store the preference update
		var returnValue = super.execute(event);		
		
		// now we can apply the filter
		new ProductFilter(TreeFilterManager.getInstance(), filterName).applyFilter();

		SnykExtendedLanguageClient.getInstance().updateConfiguration();

		return returnValue;
	}
}
