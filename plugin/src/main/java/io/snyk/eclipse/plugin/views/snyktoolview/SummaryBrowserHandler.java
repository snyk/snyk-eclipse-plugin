package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.concurrent.CompletableFuture;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;

import io.snyk.eclipse.plugin.html.StaticPageHtmlProvider;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class SummaryBrowserHandler {
	private Browser browser;
	// Last summary rendered (null => the default init page). Kept so the panel can be re-rendered
	// when the resolved theme color changes after the CSS engine has styled the view.
	private String lastSummary;

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
		lastSummary = null;
		BrowserFlashGuard.setTextFlashSafe(browser, StaticPageHtmlProvider.getInstance().getSummaryInitHtml());
	}

	public void setBrowserText(String summary) {
		lastSummary = summary;
		BrowserFlashGuard.setTextFlashSafe(browser,
				StaticPageHtmlProvider.getInstance().getFormattedSummaryHtml(summary));
	}

	/** Re-renders the current summary so it adopts a newly-resolved theme color. */
	public void refreshTheme() {
		if (browser == null || browser.isDisposed()) {
			return;
		}
		if (lastSummary == null) {
			setDefaultBrowserText();
		} else {
			setBrowserText(lastSummary);
		}
	}

}
