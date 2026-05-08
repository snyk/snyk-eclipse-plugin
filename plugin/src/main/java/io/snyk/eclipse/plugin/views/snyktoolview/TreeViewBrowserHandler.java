package io.snyk.eclipse.plugin.views.snyktoolview;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.widgets.Display;

import io.snyk.eclipse.plugin.html.EclipseThemeCssProvider;
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
		if (browser == null || browser.isDisposed()) {
			return;
		}
		if (html == null) {
			return;
		}
		Display display = browser.getDisplay();
		String themed = injectThemeCss(html, display);
		browser.setText(themed);
	}

	public void selectNode(String issueId) {
		if (browser == null || browser.isDisposed() || issueId == null || issueId.isEmpty()) {
			return;
		}
		String escaped = issueId.replace("\\", "\\\\").replace("'", "\\'");
		browser.evaluate("if(window.__selectTreeNode__){window.__selectTreeNode__('" + escaped + "');}");
	}

	public static String injectThemeCss(String html, Display display) {
		if (html == null) {
			return null;
		}
		String styleBlock = EclipseThemeCssProvider.buildStyleBlock(display);
		if (html.contains("${ideStyle}")) {
			return html.replace("${ideStyle}", styleBlock);
		}
		if (html.contains("</head>")) {
			return html.replaceFirst("(?i)</head>", styleBlock + "</head>");
		}
		return styleBlock + html;
	}
}
