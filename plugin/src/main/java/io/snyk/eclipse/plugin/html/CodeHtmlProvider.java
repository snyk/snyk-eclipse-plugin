package io.snyk.eclipse.plugin.html;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class CodeHtmlProvider extends BaseHtmlProvider {
	private static CodeHtmlProvider instance = new CodeHtmlProvider();

	public static CodeHtmlProvider getInstance() {
		synchronized (CodeHtmlProvider.class) {
			if (instance == null) {
				if (instance == null) {
					instance = new CodeHtmlProvider();
				}
			}
		}
		return instance;
	}

	@Override
	public String getInitScript() {
		String themeScript = getThemeScript();
		String initScript = super.getInitScript();
		return initScript + "\n" + """
				    function navigateToIssue(e, target) {
				        e.preventDefault();
				        var filePath = target.getAttribute('file-path');
				        var startLine = target.getAttribute('start-line');
				        var endLine = target.getAttribute('end-line');
				        var startCharacter = target.getAttribute('start-character');
				        var endCharacter = target.getAttribute('end-character');
				        window.openInEditor(filePath, startLine, endLine, startCharacter, endCharacter);
				    }
				    var navigatableLines = document.getElementsByClassName('data-flow-clickable-row');
				    for(var i = 0; i < navigatableLines.length; i++) {
				        navigatableLines[i].onclick = function(e) {
				            navigateToIssue(e, this);
				            return false;
				        };
				    }
				    if(document.getElementById('position-line')) {
				        document.getElementById('position-line').onclick = function(e) {
				            var target = navigatableLines[0];
				            if(target) {
				                navigateToIssue(e, target);
				            }
				        }
				    }
				""" + themeScript;
	}

	private String getThemeScript() {
		if (Preferences.getInstance().isTest()) {
			return "";
		}

		String themeScript = "var isDarkTheme = " + isDarkTheme() + ";\n"
				+ "document.body.classList.add(isDarkTheme ? 'dark' : 'light');";
		return themeScript;
	}

	@Override
	public String replaceCssVariables(String html) {
		String htmlStyled = super.replaceCssVariables(html);

		String linkColor = super.getColorAsHex("org.eclipse.ui.editors.hyperlinkColor", "#4C8DEF");
		String onLinkColor = super.getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_SELECTED_TEXT_COLOR", "#FFFFFF");

		// Replace CSS variables with actual color values
		htmlStyled = htmlStyled.replace("var(--example-line-removed-color)",
				super.getColorAsHex("DELETION_COLOR", "#ff0000"));
		htmlStyled = htmlStyled.replace("var(--example-line-added-color)",
				super.getColorAsHex("ADDITION_COLOR", "#00ff00"));
		htmlStyled = htmlStyled.replace("var(--button-background-color)", linkColor);
		htmlStyled = htmlStyled.replace("var(--button-text-color)", onLinkColor);
		htmlStyled = htmlStyled.replace("var(--disabled-background-color)",
				super.getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_OUTER_KEYLINE_COLOR", "#CCCCCC"));
		htmlStyled = htmlStyled.replace("var(--vscode-input-border)", linkColor);
		htmlStyled = htmlStyled.replace("var(--warning-text)", super.getColorAsHex("WARNING_TEXT_COLOR", "#000000"));
		htmlStyled = htmlStyled.replace("var(--warning-background)",
				super.getColorAsHex("WARNING_BACKGROUND_COLOR", "#c8a000"));

		htmlStyled = injectButtonOverride(htmlStyled, linkColor, onLinkColor);

		String htmlWithScripts = replaceAIFixScripts(htmlStyled);
		String htmlWithIgnoreRequest = replaceIgnoreRequestScript(htmlWithScripts);

		return htmlWithIgnoreRequest;
	}

	// Make Generate AI fix / Apply fix buttons match the outlined Create-ignore
	// style (transparent background, link-coloured border + text, fill on hover).
	private String injectButtonOverride(String html, String linkColor, String onLinkColor) {
		String override = "<style id=\"snyk-eclipse-button-override\">"
				+ "button.generate-ai-fix,button.sn-apply-fix{"
				+ "background-color:transparent;"
				+ "color:" + linkColor + ";"
				+ "border:1px solid " + linkColor + ";"
				+ "transition:background-color .15s ease-in,color .15s ease-in;"
				+ "}"
				+ "button.generate-ai-fix:hover:not(:disabled),"
				+ "button.sn-apply-fix:hover:not(:disabled){"
				+ "background-color:" + linkColor + ";"
				+ "color:" + onLinkColor + ";"
				+ "}"
				+ "</style>";
		int headEnd = html.indexOf("</head>");
		if (headEnd < 0) {
			return override + html;
		}
		return html.substring(0, headEnd) + override + html.substring(headEnd);
	}

	private String replaceIgnoreRequestScript(String htmlWithScripts) {
		String htmlWithIgnoreRequest = htmlWithScripts.replace("${ideSubmitIgnoreRequest}",
				ideSubmitIgnoreRequestScript());
		return htmlWithIgnoreRequest;
	}

	private CharSequence ideSubmitIgnoreRequestScript() {
		return "window.ideSubmitIgnoreRequest(issueId + '@|@' + ignoreType + '@|@' + ignoreExpirationDate + '@|@' + ignoreReason);\n";
	}

	private String replaceAIFixScripts(String html) {
		String htmlWithAiFixScripts = html.replace("${ideGenerateAIFix}", getGenerateAiFixScript());
		htmlWithAiFixScripts = htmlWithAiFixScripts.replace("${ideApplyAIFix}", getApplyAiFixScript());

		return htmlWithAiFixScripts;
	}

	private String getGenerateAiFixScript() {
		return "window.ideGenAIFix(issueId)\n;";
	}

	private String getApplyAiFixScript() {
		return "window.ideApplyFix(fixId);\n";
	}

}
