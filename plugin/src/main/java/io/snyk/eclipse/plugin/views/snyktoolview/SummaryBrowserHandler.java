package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.concurrent.CompletableFuture;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;

import io.snyk.eclipse.plugin.html.StaticPageHtmlProvider;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class SummaryBrowserHandler {
	private Browser browser;
	// Track the latest rendered summary so the panel can be re-rendered if the theme color
    // changes when the CSS engine styles the view. hasSummary is false while only the default
    // init page has been shown (avoids a null assignment, which the PMD ruleset rejects).
	private String lastSummary;
	private boolean hasSummary;

	public SummaryBrowserHandler(Browser browser) {
		this.browser = browser;
	}

	public void initialize() {

		new BrowserFunction(browser, "enableDelta") {
			@Override
			public Object function(Object[] arguments) {
				boolean value = false;
				if (arguments.length > 0 && arguments[0] instanceof Boolean) {
					value = (Boolean) arguments[0];
				}

				Preferences.getInstance().storeAndTrackChange(Preferences.ENABLE_DELTA, Boolean.toString(value));

				CompletableFuture.runAsync(() -> SnykExtendedLanguageClient.getInstance().updateConfiguration());

				return null;
			}
		};

		BrowserFlashGuard.install(browser);
		setDefaultBrowserText();
	}

	public void setDefaultBrowserText() {
		hasSummary = false;
		BrowserFlashGuard.setTextFlashSafe(browser, StaticPageHtmlProvider.getInstance().getSummaryInitHtml());
	}

	public void setBrowserText(String summary) {
		lastSummary = summary;
		hasSummary = true;
		BrowserFlashGuard.setTextFlashSafe(browser,
				StaticPageHtmlProvider.getInstance().getFormattedSummaryHtml(summary));
	}

	/** Re-renders the current summary so it adopts a newly-resolved theme color. */
	public void refreshTheme() {
		if (browser == null || browser.isDisposed()) {
			return;
		}
		if (hasSummary) {
			setBrowserText(lastSummary);
		} else {
			setDefaultBrowserText();
		}
	}

}
