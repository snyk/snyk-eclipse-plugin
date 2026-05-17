package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.swt.browser.Browser;

import io.snyk.eclipse.plugin.html.BaseHtmlProvider;
import io.snyk.eclipse.plugin.html.ExecuteCommandBridge;

public class TreeViewBrowserHandler {
	private Browser browser;
	private final BaseHtmlProvider htmlProvider = new BaseHtmlProvider();

	public TreeViewBrowserHandler(Browser browser) {
		this.browser = browser;
	}

	public void initialize() {
		ExecuteCommandBridge.install(browser);
		browser.setText("<html><body></body></html>");
	}

	public void setBrowserText(String html) {
		if (browser == null || browser.isDisposed()) {
			return;
		}
		if (html == null) {
			return;
		}
		browser.setText(htmlProvider.replaceCssVariables(html));
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
				.replace(" ", "\\u2028")
				.replace(" ", "\\u2029");
	}
}
