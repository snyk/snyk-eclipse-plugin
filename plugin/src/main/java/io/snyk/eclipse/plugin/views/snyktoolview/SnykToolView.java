package io.snyk.eclipse.plugin.views.snyktoolview;

import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_IN_PROGRESS;

import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class SnykToolView extends ViewPart implements ISnykToolView {
	public SnykToolView() {
	}

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "io.snyk.eclipse.plugin.views.snyktoolview";

	private Browser browser;
	private BrowserHandler browserHandler;
	private Browser summaryBrowser;
	private SummaryBrowserHandler summaryBrowserHandler;
	private Browser treeBrowser;
	private volatile TreeViewBrowserHandler treeBrowserHandler;
	private final AtomicReference<String> pendingHtml = new AtomicReference<>();

	@Override
	public void createPartControl(Composite parent) {
		SashForm horizontalSashForm = new SashForm(parent, SWT.HORIZONTAL);
		horizontalSashForm.setLayout(new FillLayout());

		SashForm verticalSashForm = new SashForm(horizontalSashForm, SWT.VERTICAL);
		verticalSashForm.setLayout(new FillLayout());

		summaryBrowser = new Browser(verticalSashForm, SWT.EDGE);
		summaryBrowserHandler = new SummaryBrowserHandler(summaryBrowser);
		summaryBrowserHandler.initialize();

		treeBrowser = new Browser(verticalSashForm, SWT.EDGE);
		treeBrowserHandler = new TreeViewBrowserHandler(treeBrowser);
		treeBrowserHandler.initialize();
		String buffered = pendingHtml.getAndSet(null);
		if (buffered != null) {
			treeBrowserHandler.setBrowserText(buffered);
		}

		verticalSashForm.setWeights(1, 3);

		browser = new Browser(horizontalSashForm, SWT.EDGE);
		browserHandler = new BrowserHandler(browser);
		browserHandler.initialize();

		horizontalSashForm.setWeights(1, 2);
	}

	@Override
	public void setFocus() {
		treeBrowser.setFocus();
	}

	@Override
	public void refreshBrowser(String status) {
		Display.getDefault().asyncExec(() -> {
			if (SCAN_STATE_IN_PROGRESS.equals(status)) {
				this.browserHandler.setScanningBrowserText();
			} else {
				this.browserHandler.setDefaultBrowserText();
			}
		});
	}

	@Override
	public void updateSummary(String summary) {
		Display.getDefault().asyncExec(() -> {
			this.summaryBrowserHandler.setBrowserText(summary);
		});
	}

	@Override
	public void toggleIgnoresButtons() {
		Display.getDefault().asyncExec(() -> {
			IMenuManager menuManager = this.getViewSite().getActionBars().getMenuManager();
			IMenuManager submenu = menuManager
					.findMenuUsingPath("io.snyk.eclipse.plugin.views.snyktoolview.filterIgnoreMenu");

			if (submenu == null) {
				return;
			}

			addCommandIfNotPresent(submenu, "io.snyk.eclipse.plugin.commands.snykShowOpenIgnored");
			addCommandIfNotPresent(submenu, "io.snyk.eclipse.plugin.commands.snykShowIgnored");

			menuManager.update(true);
		});
	}

	@Override
	public void refreshDeltaReference() {
		// Delta reference display handled by HTML tree view
	}

	@Override
	public void enableDelta() {
		// Delta toggling handled by HTML tree view
	}

	@Override
	public void disableDelta() {
		// Delta toggling handled by HTML tree view
	}

	private void addCommandIfNotPresent(IMenuManager menu, String commandId) {
		if (menu.find(commandId) == null) {
			CommandContributionItemParameter param = new CommandContributionItemParameter(PlatformUI.getWorkbench(),
					null, commandId, CommandContributionItem.STYLE_PUSH);
			CommandContributionItem item = new CommandContributionItem(param);
			menu.add(item);
		}
	}

	@SuppressWarnings("restriction")
	protected void outputCommandResult(Object result) {
		if (result != null && result instanceof Map) {
			@SuppressWarnings("unchecked")
			Map<String, Object> resultMap = (Map<String, Object>) result;
			String stdOut = resultMap.get("stdOut").toString();
			boolean exitCode = (Double) resultMap.get("exitCode") == 0;
			if (exitCode) {
				MessageParams messageParams = new MessageParams(MessageType.Info, stdOut);
				SnykExtendedLanguageClient.getInstance().showMessage(messageParams);
			} else {
				SnykLogger.logError(new RuntimeException(stdOut));
			}
		}
	}

	@Override
	public void selectTreeNode(String issueId) {
		if (issueId == null || issueId.isEmpty()) return;
		try {
			Display display = Display.getDefault();
			if (display == null || display.isDisposed()) return;
			if (treeBrowserHandler != null) {
				display.asyncExec(() -> treeBrowserHandler.selectNode(issueId));
			}
			display.asyncExec(() -> browserHandler.updateBrowserContent(issueId));
		} catch (SWTError | SWTException | UnsatisfiedLinkError | NoClassDefFoundError e) {
			SnykLogger.logInfo("No SWT Display available for selectTreeNode: " + e.getMessage());
		}
	}

	@Override
	public void updateTreeViewHtml(String html) {
		pendingHtml.set(html);
		drainPendingHtmlAsync();
	}

	private void drainPendingHtmlAsync() {
		try {
			Display display = Display.getDefault();
			if (display != null && !display.isDisposed()) {
				display.asyncExec(() -> {
					if (treeBrowserHandler == null) return;
					String buffered = pendingHtml.getAndSet(null);
					if (buffered != null) {
						treeBrowserHandler.setBrowserText(buffered);
					}
				});
			}
		} catch (SWTError | SWTException | UnsatisfiedLinkError | NoClassDefFoundError e) {
			SnykLogger.logInfo("No SWT Display available, HTML will be drained on createPartControl: " + e.getMessage());
		}
	}

}
