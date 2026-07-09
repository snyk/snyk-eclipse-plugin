package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.ProgressAdapter;
import org.eclipse.swt.browser.ProgressEvent;

/**
 * Reduces the white flash the SWT Browser shows while a new document loads. On macOS the Browser uses
 * the legacy Cocoa WebView, which paints opaque white between documents and exposes no API to set a
 * native/page background. To avoid a jarring white flash (especially in dark mode) the browser is
 * hidden immediately before {@code setText} — so its panel-colored wrapper composite shows through
 * instead of white — and revealed again once the load completes.
 */
final class BrowserFlashGuard {
	private static final int SAFETY_REVEAL_MS = 1500;

	private BrowserFlashGuard() {
	}

	/** Installs a permanent listener that reveals the browser once each document load completes. */
	static void install(Browser browser) {
		if (browser == null || browser.isDisposed()) {
			return;
		}
		browser.addProgressListener(new ProgressAdapter() {
			@Override
			public void completed(ProgressEvent event) {
				reveal(browser);
			}
		});
	}

	/**
	 * Reveals the browser once, after its first load. Use for panels that must stay interactive while
	 * they refresh (e.g. the issue tree during a scan): the browser is hidden only for the initial
	 * cold-open load and is never hidden again, so clicks are never swallowed by a hidden control.
	 */
	static void scheduleSafetyReveal(Browser browser) {
		if (browser == null || browser.isDisposed()) {
			return;
		}
		browser.getDisplay().timerExec(SAFETY_REVEAL_MS, () -> reveal(browser));
	}

	/**
	 * Hides the browser, loads the HTML, and schedules a safety reveal so the panel can never get stuck
	 * hidden if the completed event does not fire.
	 */
	static void setTextFlashSafe(Browser browser, String html) {
		if (browser == null || browser.isDisposed() || html == null) {
			return;
		}
		browser.setVisible(false);
		browser.setText(html);
		browser.getDisplay().timerExec(SAFETY_REVEAL_MS, () -> reveal(browser));
	}

	private static void reveal(Browser browser) {
		if (browser != null && !browser.isDisposed() && !browser.getVisible()) {
			browser.setVisible(true);
		}
	}
}
