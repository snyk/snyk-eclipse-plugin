package io.snyk.eclipse.plugin.views;

import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;

import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class ScanWorkspaceFolderHandler extends AbstractHandler {

	@SuppressWarnings("restriction")
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked(event);

		ISelectionService service = window.getSelectionService();
		IStructuredSelection structured = (IStructuredSelection) service.getSelection();

		Object firstElement = structured.getFirstElement();
		IProject project = null;

		if (firstElement instanceof JavaProject) {
			project = ((JavaProject) firstElement).getProject();
		}

		if (firstElement instanceof IProject) {
			project = (IProject) firstElement;
		}

		Path fullPath = ResourceUtils.getFullPath(project);

		CompletableFuture.runAsync(() -> {
			SnykExtendedLanguageClient.getInstance().triggerScan(fullPath);
		});

		return null;
	}
}
