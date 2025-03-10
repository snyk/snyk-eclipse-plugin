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

		// Replace CSS variables with actual color values
		htmlStyled = htmlStyled.replace("var(--example-line-removed-color)",
				super.getColorAsHex("DELETION_COLOR", "#ff0000"));
		htmlStyled = htmlStyled.replace("var(--example-line-added-color)",
				super.getColorAsHex("ADDITION_COLOR", "#00ff00"));
		htmlStyled = htmlStyled.replace("var(--generated-ai-fix-button-background-color)",
				super.getColorAsHex("BUTTON_COLOR", "#375578"));
		htmlStyled = htmlStyled.replace("var(--disabled-background-color)",
				super.getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_OUTER_KEYLINE_COLOR", "#CCCCCC"));
		htmlStyled = htmlStyled.replace("var(--vscode-input-border)", super.getColorAsHex("BUTTON_COLOR", "#375578"));
		htmlStyled = htmlStyled.replace("var(--warning-text)", super.getColorAsHex("WARNING_TEXT_COLOR", "#000000"));
		htmlStyled = htmlStyled.replace("var(--warning-background)", super.getColorAsHex("WARNING_BACKGROUND_COLOR", "#c8a000"));

		String htmlWithScripts = replaceAIFixScripts(htmlStyled);

		return htmlWithScripts;
	}

	private String replaceAIFixScripts(String html) {
		String htmlWithAiFixScripts = html.replace("${ideGenerateAIFix}", getGenerateAiFixScript());
		htmlWithAiFixScripts = htmlWithAiFixScripts.replace("${ideApplyAIFix}", getApplyAiFixScript());

		return htmlWithAiFixScripts;
	}

	private String getGenerateAiFixScript() {
		return "window.ideGenAIFix(generateFixQueryString)\n;";
	}

	private String getApplyAiFixScript() {
		return "window.ideApplyFix(fixId);\n";
	}

}
