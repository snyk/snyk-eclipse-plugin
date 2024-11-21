package io.snyk.eclipse.plugin.html;

import java.util.Random;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.themes.ITheme;
import org.eclipse.ui.themes.IThemeManager;

public class BaseHtmlProvider {
    public String getCss() {
        return "";
    }

    public String getJs() {
        return "";
    }

    public String getInitScript() {
        return "";
    }

    public String getNonce() {
        String allowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        Random random = new Random();
        StringBuilder nonceBuilder = new StringBuilder(32);
        for (int i = 0; i < 32; i++) {
            nonceBuilder.append(allowedChars.charAt(random.nextInt(allowedChars.length())));
        }
        return nonceBuilder.toString();
    }

    public String replaceCssVariables(String html) {
        // Build the CSS with the nonce
        String nonce = getNonce();
        String css = "<style nonce=\"" + nonce + "\">" + getCss() + "</style>";
        html = html.replace("${ideStyle}", css);
        html = html.replace("<style nonce=\"ideNonce\" data-ide-style></style>", css);
        html = html.replace("var(--default-font)", " ui-sans-serif, \"SF Pro Text\", \"Segoe UI\", \"Ubuntu\", Tahoma, Geneva, Verdana, sans-serif;");


        // Replace CSS variables with actual color values
        html = html.replace("var(--text-color)", getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_TEXT_COLOR", "#000000"));
        html = html.replace("var(--background-color)", getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_BG_START", "#FFFFFF"));
        html = html.replace("var(--border-color)", getColorAsHex( "org.eclipse.ui.workbench.ACTIVE_TAB_BORDER_COLOR", "#CCCCCC"));
        html = html.replace("var(--link-color)", getColorAsHex("org.eclipse.ui.workbench.HYPERLINK_COLOR", "#0000FF"));
        html = html.replace("var(--horizontal-border-color)", getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_HIGHLIGHT_BORDER_COLOR", "#CCCCCC"));
        html = html.replace("var(--code-background-color)", getColorAsHex("org.eclipse.ui.workbench.CODE_BACKGROUND_COLOR", "#F0F0F0"));

        // Update the HTML head
        String ideHeaders = """
                            <head>
                            <meta http-equiv='Content-Type' content='text/html; charset=unicode' />
                            <meta http-equiv='X-UA-Compatible' content='IE=edge' />
                            """;
        html = html.replace("<head>", ideHeaders);
        html = html.replace("${headerEnd}", "");

        // Replace nonce placeholders
        html = html.replace("${nonce}", nonce);
        html = html.replace("ideNonce", nonce);
        html = html.replace("${ideScript}", "");

        return html;
    }

    public String getColorAsHex(String colorKey, String defaultColor) {
    	ColorRegistry colorRegistry = getColorRegistry();
        Color color = colorRegistry.get(colorKey);
        if (color == null) {
            return defaultColor;
        } else {
            RGB rgb = color.getRGB();
            return String.format("#%02x%02x%02x", rgb.red, rgb.green, rgb.blue);
        }
    }
    
    private ColorRegistry getColorRegistry() {
        IThemeManager themeManager = PlatformUI.getWorkbench().getThemeManager();
        ITheme currentTheme = themeManager.getCurrentTheme();
        return currentTheme.getColorRegistry();
    }
}
