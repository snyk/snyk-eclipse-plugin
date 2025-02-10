package io.snyk.eclipse.plugin.html;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.themes.ITheme;
import org.eclipse.ui.themes.IThemeManager;
import org.osgi.framework.Bundle;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.ResourceUtils;

public class BaseHtmlProvider {
	private final Random random = new Random();
	private final Map<String, String> colorCache = new HashMap<>();
	private String nonce = "";

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
		if (!nonce.isEmpty()) {
			return nonce;
		}
		String allowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
		StringBuilder nonceBuilder = new StringBuilder(32);
		for (int i = 0; i < 32; i++) {
			nonceBuilder.append(allowedChars.charAt(random.nextInt(allowedChars.length())));
		}
		nonce = nonceBuilder.toString();
		return nonce;
	}

	public String getNoDescriptionHtml() {
		String snykWarningText = Platform.getResourceString(Platform.getBundle("io.snyk.eclipse.plugin"),
				"snyk.panel.auth.trust.warning.text");

		Bundle bundle = Platform.getBundle("io.snyk.eclipse.plugin");
		String base64Image = ResourceUtils.getBase64Image(bundle, "logo_snyk.png");

		var html = """
				<!DOCTYPE html>
				<html lang="en">
				<head>
				    <meta charset="UTF-8">
				    <meta name="viewport" content="width=device-width, initial-scale=1.0">
				    <style>
				        body {
				        	font-family: var(--default-font);
				            background-color: var(--background-color);
				            color: var(--text-color);
				        }
				        .container {
				            display: flex;
				            align-items: center;
				        }
				        .logo {
				            margin-right: 20px;
				        }
						a {
							color: var(--link-color)
						}

						div {
							padding: 20px
						}
				    </style>
				</head>
				<body>
				    <div class="container">
				        <img src='data:image/png;base64,%s' alt='Snyk Logo'>
				        <div>
				            <p><strong>Please rescan to see the issue description.</strong></p>
				        </div>
				    </div>
				</body>
				</html>
				""".formatted(base64Image, snykWarningText);
		return html;
	}

	public String replaceCssVariables(String html) {
		// Build the CSS with the nonce
		String nonce = getNonce();
		String css = "<style nonce=\"" + nonce + "\">" + getCss() + "</style>";
		String htmlStyled = html.replace("${ideStyle}", css);
		htmlStyled = htmlStyled.replace("<style nonce=\"ideNonce\" data-ide-style></style>", css);
		htmlStyled = htmlStyled.replace("var(--default-font)",
				" ui-sans-serif, \"SF Pro Text\", \"Segoe UI\", \"Ubuntu\", Tahoma, Geneva, Verdana, sans-serif;");
		htmlStyled = htmlStyled.replace("var(--main-font-size)", getScaledFontSize());

		// Replace CSS variables with actual color values
		htmlStyled = htmlStyled.replace("var(--text-color)",
				getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_TEXT_COLOR", "#000000"));

		htmlStyled = htmlStyled.replace("var(--ide-background-color)",
				getColorAsHex("org.eclipse.ui.workbench.ACTIVE_NOFOCUS_TAB_BG_START", "#FFFFFF"));
		htmlStyled = htmlStyled.replace("var(--background-color)",
				getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_BG_END", "#FFFFFF"));
		htmlStyled = htmlStyled.replace("var(--code-background-color)",
				getColorAsHex("org.eclipse.ui.workbench.INACTIVE_TAB_BG_START", "#F0F0F0"));
		htmlStyled = htmlStyled.replace("var(--button-color)",
				getColorAsHex("org.eclipse.ui.workbench.INACTIVE_TAB_BG_START", "#F0F0F0"));
		htmlStyled = htmlStyled.replace("var(--circle-color)",
				getColorAsHex("org.eclipse.ui.workbench.INACTIVE_TAB_BG_START", "#F0F0F0"));

		htmlStyled = htmlStyled.replace("var(--border-color)",
				getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_OUTER_KEYLINE_COLOR", "#CCCCCC"));
		htmlStyled = htmlStyled.replace("var(--link-color)", getColorAsHex("ACTIVE_HYPERLINK_COLOR", "#0000FF"));
		htmlStyled = htmlStyled.replace("var(--horizontal-border-color)",
				getColorAsHex("org.eclipse.ui.workbench.ACTIVE_TAB_OUTER_KEYLINE_COLOR", "#CCCCCC"));

		htmlStyled = htmlStyled.replace("${headerEnd}", "");
		htmlStyled = htmlStyled.replace("${nonce}", nonce);
		htmlStyled = htmlStyled.replace("ideNonce", nonce);
		htmlStyled = htmlStyled.replace("${ideScript}", "");

		return htmlStyled;
	}

	private String getScaledFontSize() {
		int defaultHeight;
		try {
			defaultHeight = getCurrentTheme().getFontRegistry().getFontData(JFaceResources.TEXT_FONT)[0].getHeight();
		} catch (IllegalStateException e) {
			defaultHeight = 13;
		}
		// Language server HTML assumes a base font size of 10px. The default Eclipse font size is 17px (13pt), so we
		// apply a scaling factor here. This ensures that HTML fonts scale correctly if the user changes the text size.
		int scaledHeight = (int) (defaultHeight / 1.7);
		return scaledHeight + "pt";
	}

	public String getColorAsHex(String colorKey, String defaultColor) {
		if (Preferences.getInstance().isTest()) {
			return "";
		}
		return colorCache.computeIfAbsent(colorKey, key -> {
			ColorRegistry colorRegistry = getColorRegistry();
			Color color = colorRegistry.get(colorKey);
			if (color == null) {
				return defaultColor;
			} else {
				RGB rgb = color.getRGB();
				return String.format("#%02x%02x%02x", rgb.red, rgb.green, rgb.blue);
			}
		});
	}

	public Boolean isDarkTheme() {
		var darkColor = getColorAsHex("org.eclipse.ui.workbench.DARK_BACKGROUND", "");
		return Boolean.valueOf(darkColor);
	}

	private ColorRegistry colorRegistry;

	private ColorRegistry getColorRegistry() {
		if (colorRegistry != null) {
			return colorRegistry;
		}
		ITheme currentTheme = getCurrentTheme();
		colorRegistry = currentTheme.getColorRegistry();
		return colorRegistry;
	}

	private ITheme currentTheme;

	public ITheme getCurrentTheme() {
		if (currentTheme != null) {
			return currentTheme;
		}
		IThemeManager themeManager = PlatformUI.getWorkbench().getThemeManager();
		currentTheme = themeManager.getCurrentTheme();
		return currentTheme;
	}

	public String getErrorHtml(String errorMessage, String path) {
		var html = """
				<!DOCTYPE html>
				<html lang="en">
				<head>
				    <meta charset="UTF-8">
				    <meta name="viewport" content="width=device-width, initial-scale=1.0">
				    <title>Snyk for Eclipse</title>
				    <style>
				        body {
				        	font-family: var(--default-font);
				            background-color: var(--background-color);
				            color: var(--text-color);
				        }
				        .container {
				            display: flex;
				            align-items: center;
				        }
				        .logo {
				            margin-right: 20px;
				        }
				    </style>
				</head>
				<body>
				    <div class="container">
				        <div>
				            <p><strong>An error occurred:</strong></p>
				            <p>
				            <table>
				            	<tr><td width="150"	>Error message:</td><td>%s</td></tr>
				            	<tr></tr>
				            	<tr><td>Path:</td><td>%s</td></tr>
				            </table>
				            </p>
				        </div>
				    </div>
				</body>
				</html>
				""".formatted(errorMessage, path);
		return replaceCssVariables(html);
	}
}
