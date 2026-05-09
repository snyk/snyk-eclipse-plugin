package io.snyk.eclipse.plugin.views.snyktoolview;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.widgets.Display;

import io.snyk.eclipse.plugin.html.EclipseThemeCssProvider;
import io.snyk.eclipse.plugin.html.ExecuteCommandBridge;

public class TreeViewBrowserHandler {
	// Matches IntelliJ's ThemeBasedStylingGenerator.CSS_PATTERN:
	// group 1 = var name from var(--name...); group 3 = name from --name: declaration
	private static final Pattern CSS_PATTERN = Pattern
			.compile("var\\(--([a-zA-Z0-9_-]+)([^)]*)?\\)|--([a-zA-Z0-9_-]+):\\s");

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
		Matcher m = CSS_PATTERN.matcher(html);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String varUsageName = m.group(1);
			String declName = m.group(3);
			String replacement;
			if (varUsageName != null && !varUsageName.isEmpty()) {
				replacement = vars.getOrDefault("--" + varUsageName, m.group(0));
			} else if (declName != null && !declName.isEmpty()) {
				String value = vars.get("--" + declName);
				replacement = value != null ? "--" + declName + ": " + value + "; " : m.group(0);
			} else {
				replacement = m.group(0);
			}
			m.appendReplacement(sb, Matcher.quoteReplacement(replacement));
		}
		m.appendTail(sb);
		return sb.toString().replace("${ideStyle}", "").replace("${ideScript}", "");
	}
}
