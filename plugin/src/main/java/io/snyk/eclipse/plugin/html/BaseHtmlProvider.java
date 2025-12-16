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

	// Suffix used to neutralize VSCode CSS variable fallbacks (e.g., "var(--vscode-foo, fallback)")
	// We replace "var(--vscode-foo," with "value; --unused:" so the fallback becomes a no-op property
	private static final String VSCODE_VAR_SUFFIX = "; --unused:";

	// Eclipse theme color keys
	private static final String THEME_INACTIVE_TAB_BG = "org.eclipse.ui.workbench.INACTIVE_TAB_BG_START";
	private static final String THEME_ACTIVE_TAB_KEYLINE = "org.eclipse.ui.workbench.ACTIVE_TAB_OUTER_KEYLINE_COLOR";
	private static final String THEME_ACTIVE_TAB_SELECTED_TEXT = "org.eclipse.ui.workbench.ACTIVE_TAB_SELECTED_TEXT_COLOR";
	private static final String THEME_ACTIVE_TAB_TEXT = "org.eclipse.ui.workbench.ACTIVE_TAB_TEXT_COLOR";
	private static final String THEME_ACTIVE_NOFOCUS_TAB_BG = "org.eclipse.ui.workbench.ACTIVE_NOFOCUS_TAB_BG_START";
	private static final String THEME_ACTIVE_TAB_BG_END = "org.eclipse.ui.workbench.ACTIVE_TAB_BG_END";
	private static final String THEME_ACTIVE_HYPERLINK = "ACTIVE_HYPERLINK_COLOR";
	private static final String THEME_DARK_BACKGROUND = "org.eclipse.ui.workbench.DARK_BACKGROUND";

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

	private static final String VSCODE_VAR_FONT_FAMILY_PREFIX = "var(--vscode-font-family,";
	private static final String VSCODE_VAR_EDITOR_FONT_FAMILY_PREFIX = "var(--vscode-editor-font-family,";
	private static final String VSCODE_VAR_FONT_SIZE_PREFIX = "var(--vscode-font-size,";
	private static final String VSCODE_VAR_EDITOR_BACKGROUND_PREFIX = "var(--vscode-editor-background,";
	private static final String VSCODE_VAR_FOREGROUND_PREFIX = "var(--vscode-foreground,";
	private static final String VSCODE_VAR_INPUT_FOREGROUND_PREFIX = "var(--vscode-input-foreground,";
	private static final String VSCODE_VAR_EDITOR_FOREGROUND_PREFIX = "var(--vscode-editor-foreground,";
	private static final String VSCODE_VAR_DISABLED_FOREGROUND_PREFIX = "var(--vscode-disabledForeground,";
	private static final String VSCODE_VAR_ERROR_FOREGROUND_PREFIX = "var(--vscode-errorForeground,";
	private static final String VSCODE_VAR_INPUT_BACKGROUND_PREFIX = "var(--vscode-input-background,";
	private static final String VSCODE_VAR_EDITOR_INACTIVE_SELECTION_BG_PREFIX = "var(--vscode-editor-inactiveSelectionBackground,";
	private static final String VSCODE_VAR_BUTTON_BACKGROUND_PREFIX = "var(--vscode-button-background,";
	private static final String VSCODE_VAR_BUTTON_FOREGROUND_PREFIX = "var(--vscode-button-foreground,";
	private static final String VSCODE_VAR_BUTTON_HOVER_BG_PREFIX = "var(--vscode-button-hoverBackground,";
	private static final String VSCODE_VAR_BUTTON_SECONDARY_BG_PREFIX = "var(--vscode-button-secondaryBackground,";
	private static final String VSCODE_VAR_BUTTON_SECONDARY_FOREGROUND_PREFIX = "var(--vscode-button-secondaryForeground,";
	private static final String VSCODE_VAR_BUTTON_SECONDARY_HOVER_BG_PREFIX = "var(--vscode-button-secondaryHoverBackground,";
	private static final String VSCODE_VAR_LIST_HOVER_BG_PREFIX = "var(--vscode-list-hoverBackground,";
	private static final String VSCODE_VAR_INPUT_BORDER_PREFIX = "var(--vscode-input-border,";
	private static final String VSCODE_VAR_PANEL_BORDER_PREFIX = "var(--vscode-panel-border,";
	private static final String VSCODE_VAR_FOCUS_BORDER_PREFIX = "var(--vscode-focusBorder,";
	private static final String VSCODE_VAR_SCROLLBAR_SLIDER_BG_PREFIX = "var(--vscode-scrollbarSlider-background,";
	private static final String VSCODE_VAR_SCROLLBAR_SLIDER_HOVER_BG_PREFIX = "var(--vscode-scrollbarSlider-hoverBackground,";
	private static final String VSCODE_VAR_SCROLLBAR_SLIDER_ACTIVE_BG_PREFIX = "var(--vscode-scrollbarSlider-activeBackground,";

	// Default fallback colors
	private static final String DEFAULT_SECTION_BG_COLOR = "#F0F0F0";
	private static final String DEFAULT_BORDER_COLOR = "#CCCCCC";

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

		htmlStyled = htmlStyled.replace(CSS_VAR_IDE_BACKGROUND_COLOR, getColorAsHex(THEME_ACTIVE_NOFOCUS_TAB_BG, "#FFFFFF"));
		htmlStyled = htmlStyled.replace(CSS_VAR_BACKGROUND_COLOR, getColorAsHex(THEME_ACTIVE_TAB_BG_END, "#FFFFFF"));
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
		htmlStyled = htmlStyled.replace(CSS_VAR_FOCUS_COLOR, getColorAsHex(THEME_ACTIVE_HYPERLINK, "#0000FF"));

		// Replace VSCode CSS variables used in LS-served HTML (settings page)
		String textColor = getColorAsHex(THEME_ACTIVE_TAB_SELECTED_TEXT, "#000000");
		String bgColor = getColorAsHex(THEME_ACTIVE_TAB_BG_END, "#FFFFFF");
		String inputBgColor = getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR);
		String borderColor = getColorAsHex(THEME_ACTIVE_TAB_KEYLINE, DEFAULT_BORDER_COLOR);
		String focusColor = getColorAsHex(THEME_ACTIVE_HYPERLINK, "#0000FF");
		String buttonBgColor = getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR);
		String sectionBgColor = getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR);

		htmlStyled = htmlStyled.replace(VSCODE_VAR_FONT_FAMILY_PREFIX, textColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_EDITOR_FONT_FAMILY_PREFIX, textColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_FONT_SIZE_PREFIX, "13px" + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_EDITOR_BACKGROUND_PREFIX, bgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_FOREGROUND_PREFIX, textColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_INPUT_FOREGROUND_PREFIX, textColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_EDITOR_FOREGROUND_PREFIX, textColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_DISABLED_FOREGROUND_PREFIX, "#808080" + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_ERROR_FOREGROUND_PREFIX, "#f48771" + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_INPUT_BACKGROUND_PREFIX, inputBgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_EDITOR_INACTIVE_SELECTION_BG_PREFIX, sectionBgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_BUTTON_BACKGROUND_PREFIX, buttonBgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_BUTTON_FOREGROUND_PREFIX, textColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_BUTTON_HOVER_BG_PREFIX, buttonBgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_BUTTON_SECONDARY_BG_PREFIX, buttonBgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_BUTTON_SECONDARY_FOREGROUND_PREFIX, textColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_BUTTON_SECONDARY_HOVER_BG_PREFIX, buttonBgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_LIST_HOVER_BG_PREFIX, sectionBgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_INPUT_BORDER_PREFIX, borderColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_PANEL_BORDER_PREFIX, borderColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_FOCUS_BORDER_PREFIX, focusColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_SCROLLBAR_SLIDER_BG_PREFIX, inputBgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_SCROLLBAR_SLIDER_HOVER_BG_PREFIX, inputBgColor + VSCODE_VAR_SUFFIX);
		htmlStyled = htmlStyled.replace(VSCODE_VAR_SCROLLBAR_SLIDER_ACTIVE_BG_PREFIX, inputBgColor + VSCODE_VAR_SUFFIX);

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
