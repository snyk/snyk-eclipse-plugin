package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.swt.browser.Browser;

import io.snyk.eclipse.plugin.html.BaseHtmlProvider;
import io.snyk.eclipse.plugin.html.ExecuteCommandBridge;

public class TreeViewBrowserHandler {
	private Browser browser;
	// The feedback banner link (styles.css: .feedback-banner-link) uses --button-text-color, which the
	// IDE resolves to the button foreground — black in the dark theme (readable on a light button, but
	// not on the banner's dark gradient). Override it to the theme text color (white in dark, dark in
	// light), matching the banner text. Injected via ${ideStyle}, which lands after the LS styles so it
	// wins by cascade order.
	private final BaseHtmlProvider htmlProvider = new BaseHtmlProvider() {
		@Override
		public String getCss() {
			return ".feedback-banner-link { color: var(--text-color); }";
		}
	};
	// Last raw HTML rendered. The LS re-pushes tree HTML on many events (often identical); each
	// full-document reload causes a visible flash, so we skip setText when nothing changed.
	private String lastRenderedHtml;

	public TreeViewBrowserHandler(Browser browser) {
		this.browser = browser;
	}

	public void initialize() {
		ExecuteCommandBridge.install(browser);
		// The tree must stay clickable while it refreshes during a scan, so it is NOT hidden on each
		// reload (that would swallow clicks). It is hidden only for the cold-open load (by the view) and
		// revealed once the first document finishes, with a safety reveal in case that event never fires.
		BrowserFlashGuard.install(browser);
		BrowserFlashGuard.scheduleSafetyReveal(browser);
		setEmptyPage();
	}

	// Theme the empty placeholder page so it does not flash white before the tree HTML arrives.
	private void setEmptyPage() {
		if (browser == null || browser.isDisposed()) {
			return;
		}
		browser.setText(htmlProvider.replaceCssVariables(
				"<html><body style=\"background-color: var(--background-color)\"></body></html>"));
	}

	public void setBrowserText(String html) {
		if (browser == null || browser.isDisposed()) {
			return;
		}
		if (html == null) {
			return;
		}
		if (html.equals(lastRenderedHtml)) {
			return;
		}
		lastRenderedHtml = html;
		browser.setText(htmlProvider.replaceCssVariables(html));
	}

	/**
	 * Re-renders the current content so it picks up a newly-resolved theme color. Bypasses the
	 * unchanged-HTML guard, since the raw HTML is identical but the resolved colors have changed.
	 */
	public void refreshTheme() {
		if (browser == null || browser.isDisposed()) {
			return;
		}
		if (lastRenderedHtml == null) {
			setEmptyPage();
			return;
		}
		String html = lastRenderedHtml;
		lastRenderedHtml = null;
		setBrowserText(html);
	}

	public void selectNode(String issueId) {
		if (browser == null || browser.isDisposed() || issueId == null || issueId.isEmpty()) {
			return;
		}
		browser.evaluate("if(window.__selectTreeNode__){window.__selectTreeNode__('" + escapeJsSingleQuotedString(issueId) + "');}");
	}

	static String escapeJsSingleQuotedString(String s) {
		return s.replace("\\", "\\\\")
				.replace("'", "\\'")
				.replace("\n", "\\n")
				.replace("\r", "\\r")
				.replace("\u2028", "\\u2028")
				.replace("\u2029", "\\u2029");
	}
}
