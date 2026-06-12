package io.snyk.eclipse.plugin.views.snyktoolview;

import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_IN_PROGRESS;

import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

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
import org.eclipse.ui.part.ViewPart;

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
		// Filter menus removed; no-op.
	}

	@Override
	public void refreshDeltaReference() {
		// Delta reference display handled by HTML tree view
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
	public void selectTreeNode(String issueId, String product) {
		if (issueId == null || issueId.isEmpty()) return;
		try {
			Display display = Display.getDefault();
			if (display == null || display.isDisposed()) return;
			display.asyncExec(() -> dispatchTreeNode(issueId, product));
		} catch (SWTError | SWTException | UnsatisfiedLinkError | NoClassDefFoundError e) {
			SnykLogger.logInfo("No SWT Display available for selectTreeNode: " + e.getMessage());
		}
	}

	void dispatchTreeNode(String issueId, String product) {
		if (treeBrowserHandler != null) {
			treeBrowserHandler.selectNode(issueId);
		}
		dispatchBrowserContent(issueId, product);
	}

	// The HTML tree has no ISelectionChangedListener, so drive the detail panel update directly.
	void dispatchBrowserContent(String issueId, String product) {
		if (browserHandler != null) {
			browserHandler.updateBrowserContent(issueId, product);
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
