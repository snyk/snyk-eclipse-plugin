package io.snyk.eclipse.plugin.html;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.themes.ITheme;
import org.eclipse.ui.themes.IThemeManager;

public class CodeHtmlProvider extends BaseHtmlProvider {
    private static CodeHtmlProvider instance = new CodeHtmlProvider();

	public static CodeHtmlProvider getInstance() {
		if (instance == null) {
			synchronized (CodeHtmlProvider.class) {
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
                // Disable Autofix and ignores
                if(document.getElementById('ai-fix-wrapper') && document.getElementById('no-ai-fix-wrapper')){
                    document.getElementById('ai-fix-wrapper').className = 'hidden';
                    document.getElementById('no-ai-fix-wrapper').className = '';
                }
                if(document.getElementsByClassName('ignore-action-container') && document.getElementsByClassName('ignore-action-container')[0]){
                    document.getElementsByClassName('ignore-action-container')[0].className = 'hidden';
                }
            """ + themeScript;
    }

    private String getThemeScript() {
        IThemeManager themeManager = PlatformUI.getWorkbench().getThemeManager();
        ITheme currentTheme = themeManager.getCurrentTheme();
        String themeId = currentTheme.getId().toLowerCase();

        boolean isDarkTheme = themeId.contains("dark");
        boolean isHighContrast = themeId.contains("highcontrast") || themeId.contains("high-contrast");

        String themeScript = "var isDarkTheme = " + isDarkTheme + ";\n" +
                             "var isHighContrast = " + isHighContrast + ";\n" +
                             "document.body.classList.add(isHighContrast ? 'high-contrast' : (isDarkTheme ? 'dark' : 'light'));";
        return themeScript;
    }

    @Override
    public String replaceCssVariables(String html) {
        html = super.replaceCssVariables(html);

        // Replace CSS variables with actual color values
        html = html.replace("var(--example-line-removed-color)", super.getColorAsHex("org.eclipse.ui.workbench.lineRemovedColor", "#ff0000"));
        html = html.replace("var(--example-line-added-color)", super.getColorAsHex("org.eclipse.ui.workbench.lineAddedColor", "#00ff00"));

        return html;
    }
}
