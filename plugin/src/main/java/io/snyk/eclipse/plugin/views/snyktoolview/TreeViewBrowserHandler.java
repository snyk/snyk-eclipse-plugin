package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

	public static String injectThemeCss(String html, Display display) {
		if (html == null) {
			return null;
		}
		Map<String, String> vars = EclipseThemeCssProvider.buildVariableMap(display);
		Pattern p = Pattern.compile("var\\(\\s*(--vscode-[^,)\\s]+)\\s*(?:,[^)]*)?\\)");
		Matcher m = p.matcher(html);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String varName = m.group(1);
			String replacement = vars.getOrDefault(varName, m.group(0));
			m.appendReplacement(sb, Matcher.quoteReplacement(replacement));
		}
		m.appendTail(sb);
		return sb.toString().replace("${ideStyle}", "").replace("${ideScript}", "");
	}
}
