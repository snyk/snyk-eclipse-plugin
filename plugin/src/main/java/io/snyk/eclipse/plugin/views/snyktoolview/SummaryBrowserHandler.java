package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.concurrent.CompletableFuture;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;

import io.snyk.eclipse.plugin.html.StaticPageHtmlProvider;
import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class SummaryBrowserHandler {
	private Browser browser;

	public SummaryBrowserHandler(Browser browser) {
		this.browser = browser;
	}

	public void initialize() {

		new BrowserFunction(browser, "totalIssues") {
			@Override
			public Object function(Object[] arguments) {
				Preferences.getInstance().store(Preferences.ENABLE_DELTA, Boolean.FALSE.toString());
				updateConfiguration();

				return null;
			}
		};

		new BrowserFunction(browser, "newIssues") {
			@Override
			public Object function(Object[] arguments) {
				Preferences.getInstance().store(Preferences.ENABLE_DELTA, Boolean.TRUE.toString());
				updateConfiguration();

				return null;
			}

		};

		setDefaultBrowserText();
	}

	private void updateConfiguration() {
		CompletableFuture.runAsync(() -> {
			// Update the Snyk Language Server configuration.
			final var lc = SnykExtendedLanguageClient.getInstance();
			lc.updateConfiguration();
		});
	}

	public void setDefaultBrowserText() {
		browser.setText(StaticPageHtmlProvider.getInstance().getSummaryInitHtml());
	}

	public void setBrowserText(String summary) {
		browser.setText(StaticPageHtmlProvider.getInstance().getFormattedSummaryHtml(summary));
	}

}
