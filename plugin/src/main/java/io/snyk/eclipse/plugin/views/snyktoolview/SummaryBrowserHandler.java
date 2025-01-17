package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;

import io.snyk.eclipse.plugin.html.StaticPageHtmlProvider;

public class SummaryBrowserHandler {
	private Browser browser;

	public SummaryBrowserHandler(Browser browser) {
		this.browser = browser;
	}

	public void initialize() {

		new BrowserFunction(browser, "showAllIssuesTab") {
			@Override
			public Object function(Object[] arguments) {
				// TODO show all scan results
				browser.execute("document.body.innerHTML += '<p>All issues tab clicked</p>';");
				return null;
			}
		};

		new BrowserFunction(browser, "showDeltaIssuesTab") {
			@Override
			public Object function(Object[] arguments) {
				// TODO launch delta scan or show delta scan results
				browser.execute("document.body.innerHTML += '<p>Delta issues tab clicked</p>';");
				return null;
			}
		};

		setDefaultBrowserText();
	}

	public void setDefaultBrowserText() {
		browser.setText(StaticPageHtmlProvider.getInstance().getSummaryInitHtml());
	}

	public void setBrowserText(String summary) {
		browser.setText(summary);
	}

}
