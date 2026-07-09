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
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.ui.part.ViewPart;

import io.snyk.eclipse.plugin.html.BaseHtmlProvider;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.SnykScanParam;

public class SnykToolView extends ViewPart implements ISnykToolView {
	public SnykToolView() {
	}

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "io.snyk.eclipse.plugin.views.snyktoolview";

	private Composite rootComposite;
	private SashForm horizontalSashForm;
	private SashForm verticalSashForm;
	// Panel-colored wrappers that host each browser (see createPartControl).
	private Composite summaryPane;
	private Composite treePane;
	private Composite detailPane;
	// Owned divider color for the sashes (a contrasting shade of the panel background). Disposed with
	// the view; recreated whenever the panel background changes.
	private Color sashColor;
	private Browser browser;
	private BrowserHandler browserHandler;
	private Browser summaryBrowser;
	private SummaryBrowserHandler summaryBrowserHandler;
	private Browser treeBrowser;
	private volatile TreeViewBrowserHandler treeBrowserHandler;
	private final AtomicReference<String> pendingHtml = new AtomicReference<>();
	private static final int TREE_RENDER_DEBOUNCE_MS = 150;
	private static final String SASH_PAINTER_KEY = "snyk.sashPainter";
	private final Runnable treeRenderRunnable = this::renderPendingTreeHtml;

	@Override
	public void createPartControl(Composite parent) {
		this.rootComposite = parent;

		// Best-effort background so the first render is not white. The e4 CSS engine styles the parent
		// (to the same grey as native trees/views) only after createPartControl returns, so this initial
		// read may be pre-theme; captureThemeBackgroundDeferred() below re-reads once styling is applied.
		Color viewBackground = parent.getBackground();
		BaseHtmlProvider.setIdeBackgroundColorHex(toHex(viewBackground));

		applyBackground(parent, viewBackground);

		horizontalSashForm = new SashForm(parent, SWT.HORIZONTAL);
		horizontalSashForm.setLayout(new FillLayout());

		verticalSashForm = new SashForm(horizontalSashForm, SWT.VERTICAL);
		verticalSashForm.setLayout(new FillLayout());

		// Sashes get a contrasting shade so the dividers between panels stay visible.
		applySashColor(viewBackground);

		// Each browser sits in a panel-colored wrapper. While a browser is hidden during a reload, the
		// wrapper shows through in the panel color instead of the browser's opaque-white default — so a
		// refresh reads as a brief dark (invisible in dark mode) transition rather than a white flash.
		summaryPane = wrapperComposite(verticalSashForm, viewBackground);
		summaryBrowser = new Browser(summaryPane, SWT.EDGE);
		applyBackground(summaryBrowser, viewBackground);
		summaryBrowser.setVisible(false);
		summaryBrowserHandler = new SummaryBrowserHandler(summaryBrowser);
		summaryBrowserHandler.initialize();

		treePane = wrapperComposite(verticalSashForm, viewBackground);
		treeBrowser = new Browser(treePane, SWT.EDGE);
		applyBackground(treeBrowser, viewBackground);
		treeBrowser.setVisible(false);
		treeBrowserHandler = new TreeViewBrowserHandler(treeBrowser);
		treeBrowserHandler.initialize();
		String buffered = pendingHtml.getAndSet(null);
		if (buffered != null) {
			treeBrowserHandler.setBrowserText(buffered);
		}

		verticalSashForm.setWeights(1, 3);

		detailPane = wrapperComposite(horizontalSashForm, viewBackground);
		browser = new Browser(detailPane, SWT.EDGE);
		applyBackground(browser, viewBackground);
		browser.setVisible(false);
		browserHandler = new BrowserHandler(browser);
		browserHandler.initialize();

		horizontalSashForm.setWeights(1, 2);

		// Re-assert the sash color on resize: the theme's CSS can repaint the Sash controls the panel
		// color again (e.g. on a reskin), and resize is the point at which that would become visible.
		ControlListener sashRecolor = ControlListener.controlResizedAdapter(e -> {
			colorSashes(horizontalSashForm, sashColor);
			colorSashes(verticalSashForm, sashColor);
		});
		horizontalSashForm.addControlListener(sashRecolor);
		verticalSashForm.addControlListener(sashRecolor);

		// Re-read the background after the CSS engine has styled the view, then re-render so every panel
		// matches the native Eclipse grey (e.g. #2F2F2F in the dark theme).
		captureThemeBackgroundDeferred();
	}

	/**
	 * The e4 CSS engine applies theme styles (including the view background that matches native trees)
	 * on a UI cycle after createPartControl. Re-read the parent background then and, if it changed,
	 * re-publish it and re-render each panel so they adopt the correct grey. Attempted twice to guard
	 * against the CSS styling landing a cycle later than the first attempt; the work is idempotent, so
	 * the second attempt is a no-op once the color has been applied.
	 */
	private void captureThemeBackgroundDeferred() {
		if (rootComposite == null || rootComposite.isDisposed()) {
			return;
		}
		rootComposite.getDisplay().asyncExec(this::applyThemeColorsIfChanged);
		rootComposite.getDisplay().timerExec(300, this::applyThemeColorsIfChanged);
	}

	private void applyThemeColorsIfChanged() {
		if (rootComposite == null || rootComposite.isDisposed()) {
			return;
		}
		Color themed = rootComposite.getBackground();
		String hex = toHex(themed);
		if (hex == null) {
			return;
		}
		// Always (re)color the sashes: the Sash child controls do not exist yet at createPartControl time,
		// and the theme's CSS may have repainted them the panel color since, so this must run even when
		// the background color itself is unchanged.
		applySashColor(themed);

		if (hex.equals(BaseHtmlProvider.getIdeBackgroundColorHex())) {
			return;
		}
		BaseHtmlProvider.setIdeBackgroundColorHex(hex);
		applyBackground(rootComposite, themed);
		applyBackground(summaryPane, themed);
		applyBackground(treePane, themed);
		applyBackground(detailPane, themed);
		applyBackground(summaryBrowser, themed);
		applyBackground(treeBrowser, themed);
		applyBackground(browser, themed);
		if (summaryBrowserHandler != null) {
			summaryBrowserHandler.refreshTheme();
		}
		if (treeBrowserHandler != null) {
			treeBrowserHandler.refreshTheme();
		}
		if (browserHandler != null) {
			browserHandler.refreshTheme();
		}
	}

	private Composite wrapperComposite(Composite parent, Color background) {
		Composite wrapper = new Composite(parent, SWT.NONE);
		wrapper.setLayout(new FillLayout());
		applyBackground(wrapper, background);
		return wrapper;
	}

	private String toHex(Color color) {
		if (color == null) {
			return null;
		}
		RGB rgb = color.getRGB();
		return String.format("#%02x%02x%02x", rgb.red, rgb.green, rgb.blue);
	}

	// Set the native widget background so the area shown before/while HTML loads (and the gaps around
	// the browsers) matches the theme instead of flashing white — jarring in dark mode.
	private void applyBackground(Control control, Color background) {
		if (control != null && !control.isDisposed() && background != null) {
			control.setBackground(background);
		}
	}

	/**
	 * Paints the SashForm dividers a contrasting shade of the panel background so they stay visible
	 * (setting them to the panel color made them disappear). Owns the created Color and disposes the
	 * previous one.
	 */
	private void applySashColor(Color panelBackground) {
		if (panelBackground == null || rootComposite == null || rootComposite.isDisposed()) {
			return;
		}
		Color previous = sashColor;
		sashColor = createDividerColor(rootComposite.getDisplay(), panelBackground);
		applyBackground(horizontalSashForm, sashColor);
		applyBackground(verticalSashForm, sashColor);
		// Also color the Sash child controls directly: the theme's `.MPart Sash` CSS rule paints them the
		// panel color (making them invisible), overriding the SashForm composite background set above.
		colorSashes(horizontalSashForm, sashColor);
		colorSashes(verticalSashForm, sashColor);
		if (previous != null && !previous.isDisposed()) {
			previous.dispose();
		}
	}

	private void colorSashes(SashForm form, Color color) {
		if (form == null || form.isDisposed()) {
			return;
		}
		for (Control child : form.getChildren()) {
			if (child instanceof Sash) {
				child.setBackground(color);
				// The theme's CSS repaints the Sash the panel color, and a plain setBackground does not
				// survive its paint timing (the divider only appeared after the user interacted with it).
				// Install a PaintListener that draws the divider color on top on every paint — this fires
				// on the natural first paint when the view opens, so no interaction is needed.
				if (child.getData(SASH_PAINTER_KEY) == null) {
					child.setData(SASH_PAINTER_KEY, Boolean.TRUE);
					child.addPaintListener(this::paintSash);
				}
				child.redraw();
			}
		}
	}

	private void paintSash(PaintEvent event) {
		if (sashColor == null || sashColor.isDisposed() || !(event.widget instanceof Control)) {
			return;
		}
		Point size = ((Control) event.widget).getSize();
		event.gc.setBackground(sashColor);
		event.gc.fillRectangle(0, 0, size.x, size.y);
	}

	/** Lightens a dark background / darkens a light one by a fixed step to yield a visible divider. */
	private Color createDividerColor(Display display, Color background) {
		RGB rgb = background.getRGB();
		double luminance = (0.299 * rgb.red + 0.587 * rgb.green + 0.114 * rgb.blue) / 255.0;
		int step = luminance < 0.5 ? 32 : -28;
		int r = Math.min(255, Math.max(0, rgb.red + step));
		int g = Math.min(255, Math.max(0, rgb.green + step));
		int b = Math.min(255, Math.max(0, rgb.blue + step));
		return new Color(display, r, g, b);
	}

	@Override
	public void dispose() {
		if (sashColor != null && !sashColor.isDisposed()) {
			sashColor.dispose();
		}
		super.dispose();
	}

	@Override
	public void setFocus() {
		treeBrowser.setFocus();
	}

	@Override
	public void refreshBrowser(SnykScanParam param) {
		Display.getDefault().asyncExec(() -> {
			if (param != null && SCAN_STATE_IN_PROGRESS.equals(param.getStatus())) {
				this.browserHandler.setScanningBrowserText();
			} else if (param != null && param.getPresentableError() != null) {
				String errorHtml = new BaseHtmlProvider().getErrorHtml(param.getPresentableError());
				this.browserHandler.setBrowserText(errorHtml);
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
		if (Preferences.getInstance().isTest()) return;
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
		if (Preferences.getInstance().isTest()) return;
		try {
			Display display = Display.getDefault();
			if (display != null && !display.isDisposed()) {
				// Debounce: the LS pushes many tree-HTML updates during a scan and each reload flashes the
				// browser. Rescheduling the same runnable coalesces a burst into a single render once the
				// updates settle. timerExec must run on the UI thread, hence the asyncExec hop.
				display.asyncExec(() -> display.timerExec(TREE_RENDER_DEBOUNCE_MS, treeRenderRunnable));
			}
		} catch (SWTError | SWTException | UnsatisfiedLinkError | NoClassDefFoundError e) {
			SnykLogger.logInfo("No SWT Display available, HTML will be drained on createPartControl: " + e.getMessage());
		}
	}

	private void renderPendingTreeHtml() {
		if (treeBrowserHandler == null) {
			return;
		}
		String buffered = pendingHtml.getAndSet(null);
		if (buffered != null) {
			treeBrowserHandler.setBrowserText(buffered);
		}
	}

}
