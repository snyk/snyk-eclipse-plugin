package io.snyk.eclipse.plugin.html;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import org.apache.commons.text.StringEscapeUtils;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.themes.ITheme;
import org.eclipse.ui.themes.IThemeManager;
import org.osgi.framework.Bundle;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.languageserver.protocolextension.messageObjects.PresentableError;

public class BaseHtmlProvider {
	private final Random random = new Random();
	private final Map<String, String> colorCache = new HashMap<>();
	private String nonce = "";


	// Eclipse theme color keys
	private static final String THEME_INACTIVE_TAB_BG = "org.eclipse.ui.workbench.INACTIVE_TAB_BG_START";
	private static final String THEME_ACTIVE_TAB_KEYLINE = "org.eclipse.ui.workbench.ACTIVE_TAB_OUTER_KEYLINE_COLOR";
	private static final String THEME_ACTIVE_TAB_SELECTED_TEXT = "org.eclipse.ui.workbench.ACTIVE_TAB_SELECTED_TEXT_COLOR";
	private static final String THEME_ACTIVE_TAB_TEXT = "org.eclipse.ui.workbench.ACTIVE_TAB_TEXT_COLOR";
	private static final String THEME_ACTIVE_NOFOCUS_TAB_BG = "org.eclipse.ui.workbench.ACTIVE_NOFOCUS_TAB_BG_START";
	private static final String THEME_ACTIVE_TAB_BG_END = "org.eclipse.ui.workbench.ACTIVE_TAB_BG_END";
	private static final String THEME_ACTIVE_HYPERLINK = "ACTIVE_HYPERLINK_COLOR";
	private static final String THEME_DARK_BACKGROUND = "org.eclipse.ui.workbench.DARK_BACKGROUND";
	private static final String THEME_HYPERLINK_COLOR = "org.eclipse.ui.editors.hyperlinkColor";

	private static final String CSS_VAR_DEFAULT_FONT = "var(--default-font)";
	private static final String CSS_VAR_MAIN_FONT_SIZE = "var(--main-font-size)";
	private static final String CSS_VAR_TEXT_COLOR = "var(--text-color)";
	private static final String CSS_VAR_DIMMED_TEXT_COLOR = "var(--dimmed-text-color)";
	private static final String CSS_VAR_IDE_BACKGROUND_COLOR = "var(--ide-background-color)";
	private static final String CSS_VAR_BACKGROUND_COLOR = "var(--background-color)";
	private static final String CSS_VAR_CODE_BACKGROUND_COLOR = "var(--code-background-color)";
	private static final String CSS_VAR_BUTTON_COLOR = "var(--button-color)";
	private static final String CSS_VAR_CIRCLE_COLOR = "var(--circle-color)";
	private static final String CSS_VAR_BORDER_COLOR = "var(--border-color)";
	private static final String CSS_VAR_INPUT_BORDER = "var(--input-border)";
	private static final String CSS_VAR_LINK_COLOR = "var(--link-color)";
	private static final String CSS_VAR_HORIZONTAL_BORDER_COLOR = "var(--horizontal-border-color)";
	private static final String CSS_VAR_SECTION_BACKGROUND_COLOR = "var(--section-background-color)";
	private static final String CSS_VAR_INPUT_BACKGROUND_COLOR = "var(--input-background-color)";
	private static final String CSS_VAR_FOCUS_COLOR = "var(--focus-color)";

	// CSS variables used in LS-served HTML (styles.css from snyk-ls)
	private static final String CSS_VAR_LS_BACKGROUND_COLOR = "var(--background-color)";
	private static final String CSS_VAR_LS_TEXT_COLOR = "var(--text-color)";
	private static final String CSS_VAR_LS_INPUT_FOREGROUND = "var(--input-foreground)";
	private static final String CSS_VAR_LS_EDITOR_FOREGROUND = "var(--editor-foreground)";
	private static final String CSS_VAR_LS_DISABLED_FOREGROUND = "var(--disabled-foreground)";
	private static final String CSS_VAR_LS_ERROR_FOREGROUND = "var(--error-foreground)";
	private static final String CSS_VAR_LS_INPUT_BACKGROUND = "var(--input-background)";
	private static final String CSS_VAR_LS_SECTION_BACKGROUND = "var(--section-background)";
	private static final String CSS_VAR_LS_BUTTON_BACKGROUND_COLOR = "var(--button-background-color)";
	private static final String CSS_VAR_LS_BUTTON_FOREGROUND = "var(--button-foreground)";
	private static final String CSS_VAR_LS_BUTTON_HOVER_BACKGROUND = "var(--button-hover-background)";
	private static final String CSS_VAR_LS_BUTTON_SECONDARY_BACKGROUND = "var(--button-secondary-background)";
	private static final String CSS_VAR_LS_BUTTON_SECONDARY_FOREGROUND = "var(--button-secondary-foreground)";
	private static final String CSS_VAR_LS_BUTTON_SECONDARY_HOVER_BACKGROUND = "var(--button-secondary-hover-background)";
	private static final String CSS_VAR_LS_LIST_HOVER_BACKGROUND = "var(--list-hover-background)";
	private static final String CSS_VAR_LS_INPUT_BORDER = "var(--input-border)";
	private static final String CSS_VAR_LS_BORDER_COLOR = "var(--border-color)";
	private static final String CSS_VAR_LS_FOCUS_BORDER = "var(--focus-border)";
	private static final String CSS_VAR_LS_SCROLLBAR_BACKGROUND = "var(--scrollbar-background)";
	private static final String CSS_VAR_LS_SCROLLBAR_HOVER_BACKGROUND = "var(--scrollbar-hover-background)";
	private static final String CSS_VAR_LS_SCROLLBAR_ACTIVE_BACKGROUND = "var(--scrollbar-active-background)";


	// Default fallback colors
	private static final String DEFAULT_SECTION_BG_COLOR = "#F0F0F0";
	private static final String DEFAULT_BORDER_COLOR = "#CCCCCC";
	private static final String DEFAULT_WHITE_COLOR = "#FFFFFF";

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
		return replaceCssVariables(html, true);
	}

	public String replaceCssVariables(String html, boolean useRelativeFontSize) {
		// Build the CSS with the nonce
		String nonce = getNonce();
		String css = "<style nonce=\"" + nonce + "\">" + getCss() + "</style>";
		String htmlStyled = html.replace("${ideStyle}", css);
		htmlStyled = htmlStyled.replace("<style nonce=\"ideNonce\" data-ide-style></style>", css);
		htmlStyled = htmlStyled.replace(CSS_VAR_DEFAULT_FONT,
				" ui-sans-serif, \"SF Pro Text\", \"Segoe UI\", \"Ubuntu\", Tahoma, Geneva, Verdana, sans-serif;");
		if (useRelativeFontSize) {
			htmlStyled = htmlStyled.replace(CSS_VAR_MAIN_FONT_SIZE, getRelativeFontSize(getDefaultFontSize()));
		} else {
			htmlStyled = htmlStyled.replace(CSS_VAR_MAIN_FONT_SIZE, "13px");
		}

		// Replace CSS variables with actual color values
		htmlStyled = htmlStyled.replace(CSS_VAR_TEXT_COLOR, getColorAsHex(THEME_ACTIVE_TAB_SELECTED_TEXT, "#000000"));
		htmlStyled = htmlStyled.replace(CSS_VAR_DIMMED_TEXT_COLOR, getColorAsHex(THEME_ACTIVE_TAB_TEXT, "#4F5456"));

		htmlStyled = htmlStyled.replace(CSS_VAR_IDE_BACKGROUND_COLOR, getColorAsHex(THEME_ACTIVE_NOFOCUS_TAB_BG, DEFAULT_WHITE_COLOR));
		htmlStyled = htmlStyled.replace(CSS_VAR_BACKGROUND_COLOR, getColorAsHex(THEME_ACTIVE_TAB_BG_END, DEFAULT_WHITE_COLOR));
		htmlStyled = htmlStyled.replace(CSS_VAR_CODE_BACKGROUND_COLOR,
				getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR));
		htmlStyled = htmlStyled.replace(CSS_VAR_BUTTON_COLOR,
				getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR));
		htmlStyled = htmlStyled.replace(CSS_VAR_CIRCLE_COLOR,
				getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR));
		htmlStyled = htmlStyled.replace(CSS_VAR_BORDER_COLOR,
				getColorAsHex(THEME_ACTIVE_TAB_KEYLINE, DEFAULT_BORDER_COLOR));
		htmlStyled = htmlStyled.replace(CSS_VAR_INPUT_BORDER,
				getColorAsHex(THEME_ACTIVE_TAB_KEYLINE, DEFAULT_BORDER_COLOR));
		htmlStyled = htmlStyled.replace(CSS_VAR_LINK_COLOR, getColorAsHex(THEME_ACTIVE_HYPERLINK, "#0000FF"));
		htmlStyled = htmlStyled.replace(CSS_VAR_HORIZONTAL_BORDER_COLOR,
				getColorAsHex(THEME_ACTIVE_TAB_KEYLINE, DEFAULT_BORDER_COLOR));

		// Additional variables for fallback HTML
		htmlStyled = htmlStyled.replace(CSS_VAR_SECTION_BACKGROUND_COLOR,
				getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR));
		htmlStyled = htmlStyled.replace(CSS_VAR_INPUT_BACKGROUND_COLOR,
				getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR));
		htmlStyled = htmlStyled.replace(CSS_VAR_FOCUS_COLOR, getColorAsHex(THEME_ACTIVE_TAB_KEYLINE, DEFAULT_BORDER_COLOR));

		// Replace CSS variables used in LS-served HTML (settings page)
		// Get Eclipse theme colors
		String textColor = getColorAsHex(THEME_ACTIVE_TAB_SELECTED_TEXT, "#cccccc");
		String bgColor = getColorAsHex(THEME_ACTIVE_TAB_BG_END, "#1e1e1e");
		String inputBgColor = getColorAsHex(THEME_INACTIVE_TAB_BG, "#3c3c3c");
		String borderColor = getColorAsHex(THEME_ACTIVE_TAB_KEYLINE, "#454545");
		String focusColor = getColorAsHex(THEME_ACTIVE_TAB_KEYLINE, "#007acc");
		String sectionBgColor = getColorAsHex(THEME_INACTIVE_TAB_BG, "#2a2d2e");

		// Button colors: Use Eclipse hyperlink color for primary, inactive tab for secondary
		String buttonBgColor = getColorAsHex(THEME_HYPERLINK_COLOR, "#419cff");
		String buttonFgColor = "#ffffff";
		String buttonHoverBgColor = adjustBrightness(buttonBgColor, 1.15f);
		String buttonSecondaryBgColor = getColorAsHex(THEME_INACTIVE_TAB_BG, "#3b4042");
		String buttonSecondaryHoverBgColor = adjustBrightness(buttonSecondaryBgColor, 1.15f);

		// Replace LS CSS variables with Eclipse theme values
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BACKGROUND_COLOR, bgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_TEXT_COLOR, textColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_INPUT_FOREGROUND, textColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_EDITOR_FOREGROUND, textColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_DISABLED_FOREGROUND, "#808080");
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_ERROR_FOREGROUND, "#f48771");
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_INPUT_BACKGROUND, inputBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_SECTION_BACKGROUND, sectionBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_BACKGROUND_COLOR, buttonBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_FOREGROUND, buttonFgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_HOVER_BACKGROUND, buttonHoverBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_SECONDARY_BACKGROUND, buttonSecondaryBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_SECONDARY_FOREGROUND, textColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_SECONDARY_HOVER_BACKGROUND, buttonSecondaryHoverBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_LIST_HOVER_BACKGROUND, "rgba(255, 255, 255, 0.05)");
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_INPUT_BORDER, borderColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BORDER_COLOR, borderColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_FOCUS_BORDER, focusColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_SCROLLBAR_BACKGROUND, inputBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_SCROLLBAR_HOVER_BACKGROUND, adjustBrightness(inputBgColor, 1.1f));
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_SCROLLBAR_ACTIVE_BACKGROUND, adjustBrightness(inputBgColor, 1.2f));

		htmlStyled = htmlStyled.replace("${headerEnd}", "");
		htmlStyled = htmlStyled.replace("${nonce}", nonce);
		htmlStyled = htmlStyled.replaceAll("ideNonce", nonce);
		htmlStyled = htmlStyled.replace("${ideScript}", "");

		return htmlStyled;
	}

	private int getDefaultFontSize() {
		int fontSize =  13;
		try {
			fontSize = getCurrentTheme().getFontRegistry().getFontData(JFaceResources.TEXT_FONT)[0].getHeight();
		} catch (IllegalStateException e) {
			SnykLogger.logInfo("cannot get default font-size from Eclipse, using default (13)");
		}
		return fontSize;
	}

    // Utility function to scale Eclipse fonts appropriately for use in HTML elements that have been designed with
    // px values in mind.
	private String getRelativeFontSize(int inputFontSizePt) {
		// Target size is the base size for which the HTML element was designed.
		int targetSizePx = 10;
		int startingFontSizePt = inputFontSizePt > 0 ? inputFontSizePt : getDefaultFontSize();

		// FontRegistry uses pt sizes, not px, so we convert here, using standard web values from
		// https://www.w3.org/TR/css3-values/#absolute-lengths
		double pxToPtMultiplier = 72.0 / 96.0;
		double targetSizePt = targetSizePx * pxToPtMultiplier;

		// CSS allows 3 decimal places of precision for calculations.
		return String.format("%.3frem", targetSizePt / startingFontSizePt);
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

	/**
	 * Adjusts the brightness of a hex color by a given factor.
	 * @param hexColor The hex color string (e.g., "#0e639c")
	 * @param factor Brightness multiplier (> 1.0 for lighter, < 1.0 for darker)
	 * @return Adjusted hex color string
	 */
	private String adjustBrightness(String hexColor, float factor) {
		if (hexColor == null || hexColor.isEmpty() || !hexColor.startsWith("#")) {
			return hexColor;
		}

		try {
			// Parse hex color
			String hex = hexColor.substring(1);
			int r = Integer.parseInt(hex.substring(0, 2), 16);
			int g = Integer.parseInt(hex.substring(2, 4), 16);
			int b = Integer.parseInt(hex.substring(4, 6), 16);

			// Adjust brightness
			r = Math.min(255, Math.max(0, (int)(r * factor)));
			g = Math.min(255, Math.max(0, (int)(g * factor)));
			b = Math.min(255, Math.max(0, (int)(b * factor)));

			return String.format("#%02x%02x%02x", r, g, b);
		} catch (NumberFormatException | IndexOutOfBoundsException e) {
			SnykLogger.logError(new Exception("Failed to adjust brightness for color: " + hexColor, e));
			return hexColor;
		}
	}

	public Boolean isDarkTheme() {
		var darkColor = getColorAsHex(THEME_DARK_BACKGROUND, "");
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
	public String getErrorHtml(PresentableError presentableError) {
		StringBuilder errorDetails = new StringBuilder(256);

		if (presentableError == null) {
			errorDetails.append("<tr><td><strong>error:</strong></td><td>Unknown error</td></tr>");
		} else {
			// Filter out showNotification and treeNodeSuffix - they're not for display
			if (presentableError.getCode() != null && presentableError.getCode() != 0) {
				errorDetails.append(String.format("<tr><td><strong>code:</strong></td><td>%d</td></tr>%n", presentableError.getCode()));
			}

			if (presentableError.getError() != null && !presentableError.getError().isBlank()) {
				String escapedError = StringEscapeUtils.escapeHtml4(presentableError.getError());
				errorDetails.append(String.format("<tr><td><strong>error:</strong></td><td>%s</td></tr>%n", escapedError));
			}

			if (presentableError.getPath() != null && !presentableError.getPath().isBlank()) {
				String escapedPath = StringEscapeUtils.escapeHtml4(presentableError.getPath());
				errorDetails.append(String.format("<tr><td><strong>path:</strong></td><td>%s</td></tr>%n", escapedPath));
			}

			if (presentableError.getCommand() != null && !presentableError.getCommand().isBlank()) {
				String escapedCommand = StringEscapeUtils.escapeHtml4(presentableError.getCommand());
				errorDetails.append(String.format("<tr><td><strong>command:</strong></td><td>%s</td></tr>%n", escapedCommand));
			}
		}

		var html = String.format("""
				<!DOCTYPE html>
				<html lang="en">
				<head>
					<meta http-equiv='Content-Type' content='text/html; charset=unicode' />
				    <meta charset="UTF-8">
				    <meta name="viewport" content="width=device-width, initial-scale=1.0">
				    <meta http-equiv="Content-Security-Policy" content="script-src 'self' 'nonce-ideNonce'; style-src 'self' 'nonce-ideNonce';">
				    <title>Snyk for Eclipse</title>
				    <style nonce=ideNonce>
				        body {
				        	font-family: var(--default-font);
				            background-color: var(--background-color);
				            color: var(--text-color);
				        }
				        .container {
				            display: flex;
				            align-items: center;
				        }
				        table {
				            border-spacing: 10px 5px;
				        }
				        td {
				            vertical-align: top;
				            padding: 5px;
				        }
				        td:first-child {
				            width: 100px;
				        }
				    </style>
				</head>
				<body>
				    <div class="container">
				        <div>
				            <table>
				            %s
				            </table>
				        </div>
				    </div>
				</body>
				</html>
				""", errorDetails.toString());
		return replaceCssVariables(html);
	}
}
