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
    public String getCss() {
        return super.getCss() + "\n" + """
                 .identifiers {
                   padding-bottom: 20px;
                 }
                 .data-flow-table {
                   background-color: var(--code-background-color);
                   border: 1px solid transparent;
                 }
                 .tabs-nav {
                   margin: 21px 0 -21px;
                 }
                 .light .dark-only,
                 .high-contrast.high-contrast-light .dark-only {
                   display: none;
                 }

                 .dark .light-only,
                 .high-contrast:not(.high-contrast-light) .light-only {
                   display: none;
                 }
                 .tab-item {
                   cursor: pointer;
                   display: inline-block;
                   padding: 5px 10px;
                   border-bottom: 1px solid transparent;
                   color: var(--text-color);
                   text-transform: uppercase;
                 }
                 
                 .tab-item:hover {
                   /* Add hover styles if needed */
                 }
                 
                 .tab-item.is-selected {
                   border-bottom: 3px solid var(--link-color);
                 }
                 
                 .tab-content {
                   display: none;
                 }
                 
                 .tab-content.is-selected {
                   display: block;
                 }
                 .removed {
                   background-color: var(--line-removed);
                   color: #fff;
                 }
                 .lesson-link {
                   margin-left: 3px;
                 }
                 .added {
                   background-color: var(--line-added);
                   color: #fff;
                 }
                 .arrow {
                     cursor: pointer;
                     width: 20px;
                     height: 20px;
                     padding: 4px;
                     border-radius: 4px;
                     text-align: center;
                     line-height: 1;
                 }
                 .example {
                     background-color: var(--container-background-color);
                 }
            """;
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
        html = html.replace("var(--line-removed)", super.getColorAsHex("org.eclipse.ui.workbench.lineRemovedColor", "#ff0000"));
        html = html.replace("var(--line-added)", super.getColorAsHex("org.eclipse.ui.workbench.lineAddedColor", "#00ff00"));

        return html;
    }
}
