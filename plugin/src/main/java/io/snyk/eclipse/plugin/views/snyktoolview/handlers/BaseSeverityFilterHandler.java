package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;

public abstract class BaseSeverityFilterHandler extends BaseHandler implements IElementUpdater {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		super.execute(event);
		return null;
	}
}