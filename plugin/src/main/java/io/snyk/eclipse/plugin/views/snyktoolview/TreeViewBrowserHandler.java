package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.swt.browser.Browser;

import io.snyk.eclipse.plugin.html.ExecuteCommandBridge;

public class TreeViewBrowserHandler {
	private Browser browser;

	public TreeViewBrowserHandler(Browser browser) {
		this.browser = browser;
	}

	public void initialize() {
		ExecuteCommandBridge.install(browser);
		browser.setText("<html><body></body></html>");
	}

	public void setBrowserText(String html) {
		browser.setText(html);
	}
}
