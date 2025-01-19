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

		new BrowserFunction(browser, "showAllIssuesTab") {
			@Override
			public Object function(Object[] arguments) {
				Preferences.getInstance().store(Preferences.ENABLE_DELTA, Boolean.FALSE.toString());
				updateConfiguration();

				// TODO remove this when we get the HTML from Snyk Language Server
				browser.execute("document.body.innerHTML += '<p>All issues tab clicked</p>';");

				return null;
			}
		};

		new BrowserFunction(browser, "showDeltaIssuesTab") {
			@Override
			public Object function(Object[] arguments) {
				Preferences.getInstance().store(Preferences.ENABLE_DELTA, Boolean.TRUE.toString());
				updateConfiguration();

				// TODO remove this when we get the HTML from Snyk Language Server
				browser.execute("document.body.innerHTML += '<p>Delta issues tab clicked</p>';");

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

			// TODO do we want to start scan here?
			lc.triggerScan(null);
		});
	}

	public void setDefaultBrowserText() {
		browser.setText(StaticPageHtmlProvider.getInstance().getSummaryInitHtml2());
	}

	public void setBrowserText(String summary) {
		browser.setText(summary);
	}

}
