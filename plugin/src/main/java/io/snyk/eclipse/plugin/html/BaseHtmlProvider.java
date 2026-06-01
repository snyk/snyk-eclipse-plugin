package io.snyk.eclipse.plugin.html;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
	// Note: --background-color and --text-color are intentionally absent here — Stage 1 literals above
	// already replace those tokens before Stage 2 runs, so entries here would be dead no-ops.
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
	// --input-border and --border-color omitted: Stage 1 replaces those tokens before Stage 2 runs.
	private static final String CSS_VAR_LS_FOCUS_BORDER = "var(--focus-border)";
	private static final String CSS_VAR_LS_SCROLLBAR_BACKGROUND = "var(--scrollbar-background)";
	private static final String CSS_VAR_LS_SCROLLBAR_HOVER_BACKGROUND = "var(--scrollbar-hover-background)";
	private static final String CSS_VAR_LS_SCROLLBAR_ACTIVE_BACKGROUND = "var(--scrollbar-active-background)";


	// Matches var(--foo) and var(--foo, simple-fallback). Excludes fallbacks containing parens
	// (e.g. rgba(...)) so nested-paren fallbacks are left untouched rather than mis-matched.
	private static final Pattern CSS_VAR_PATTERN = Pattern.compile("var\\(--([a-zA-Z0-9_-]+)(?:,[^()]*)?\\)");

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
		invalidateCacheOnThemeChange();

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

		boolean dark = Boolean.TRUE.equals(isDarkTheme());

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
		htmlStyled = htmlStyled.replace(CSS_VAR_LINK_COLOR, getColorAsHex(THEME_ACTIVE_HYPERLINK, "#0066cc"));
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
		String textColor = getColorAsHex(THEME_ACTIVE_TAB_SELECTED_TEXT, "#000000");
		String bgColor = getColorAsHex(THEME_ACTIVE_TAB_BG_END, DEFAULT_WHITE_COLOR);
		String inputBgColor = getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR);
		String borderColor = getColorAsHex(THEME_ACTIVE_TAB_KEYLINE, DEFAULT_BORDER_COLOR);
		String focusColor = getColorAsHex(THEME_ACTIVE_HYPERLINK, "#0066cc");
		String sectionBgColor = getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR);

		// Error/warning foreground: #f48771 is dark-theme salmon (2.9:1 on white, fails AA).
		// Use a darker red on light backgrounds to meet WCAG AA (7.1:1 on white).
		String errorFg = dark ? "#f48771" : "#a31515";

		// List hover: white-at-5% is invisible on light backgrounds; flip to black-at-5%.
		String listHoverBg = dark ? "rgba(255, 255, 255, 0.05)" : "rgba(0, 0, 0, 0.05)";

		// Button colors: Use Eclipse hyperlink color for primary, inactive tab for secondary
		String buttonBgColor = getColorAsHex(THEME_HYPERLINK_COLOR, "#0000FF");
		String buttonFgColor = pickReadableForeground(buttonBgColor);
		String buttonHoverBgColor = adjustForHover(buttonBgColor, dark);
		String buttonSecondaryBgColor = getColorAsHex(THEME_INACTIVE_TAB_BG, DEFAULT_SECTION_BG_COLOR);
		String buttonSecondaryHoverBgColor = adjustForHover(buttonSecondaryBgColor, dark);

		// Replace LS CSS variables with Eclipse theme values.
		// Note: --background-color, --text-color, --border-color, --input-border are NOT replaced here
		// because Stage 1 (literal replacements above) already consumed those tokens.
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_INPUT_FOREGROUND, textColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_EDITOR_FOREGROUND, textColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_DISABLED_FOREGROUND, "#808080");
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_ERROR_FOREGROUND, errorFg);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_INPUT_BACKGROUND, inputBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_SECTION_BACKGROUND, sectionBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_BACKGROUND_COLOR, buttonBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_FOREGROUND, buttonFgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_HOVER_BACKGROUND, buttonHoverBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_SECONDARY_BACKGROUND, buttonSecondaryBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_SECONDARY_FOREGROUND, textColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_BUTTON_SECONDARY_HOVER_BACKGROUND, buttonSecondaryHoverBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_LIST_HOVER_BACKGROUND, listHoverBg);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_FOCUS_BORDER, focusColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_SCROLLBAR_BACKGROUND, inputBgColor);
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_SCROLLBAR_HOVER_BACKGROUND, adjustForHover(inputBgColor, dark));
		htmlStyled = htmlStyled.replace(CSS_VAR_LS_SCROLLBAR_ACTIVE_BACKGROUND, adjustBrightness(inputBgColor, dark ? 1.2f : 0.8f));

		// Single-pass regex replacement for var(--vscode-*) tokens not handled by explicit replacements above.
		// Mirrors IntelliJ's ThemeBasedStylingGenerator approach to ensure tokens without CSS fallbacks
		// (e.g. scrollbar rules in snyk-ls styles.css) are resolved to Eclipse theme colors.
		//
		// INVARIANT: keys added here must NOT overlap with the CSS_VAR_LS_* literal constants replaced above.
		// Overlapping keys would produce a no-op (the literal replacement already consumed the token before
		// the regex runs). Review this map alongside snyk-ls styles.css whenever REQUIRED_LS_PROTOCOL_VERSION
		// is bumped — token renames in the LS HTML silently leave unresolved vars if this map is not updated.
		String dimmedTextColor = getColorAsHex(THEME_ACTIVE_TAB_TEXT, "#4F5456");
		String inactiveSelectionBg = adjustForHover(inputBgColor, dark);

		Map<String, String> vsCodeVarMap = new HashMap<>();
		vsCodeVarMap.put("vscode-editor-background", bgColor);
		vsCodeVarMap.put("vscode-editor-foreground", textColor);
		vsCodeVarMap.put("vscode-foreground", textColor);
		vsCodeVarMap.put("vscode-descriptionForeground", dimmedTextColor);
		vsCodeVarMap.put("vscode-panel-background", bgColor);
		vsCodeVarMap.put("vscode-panel-border", borderColor);
		vsCodeVarMap.put("vscode-sideBar-background", bgColor);
		vsCodeVarMap.put("vscode-sideBar-foreground", textColor);
		vsCodeVarMap.put("vscode-tab-activeBackground", bgColor);
		vsCodeVarMap.put("vscode-input-background", inputBgColor);
		vsCodeVarMap.put("vscode-input-foreground", textColor);
		vsCodeVarMap.put("vscode-textLink-foreground", focusColor);
		vsCodeVarMap.put("vscode-button-background", buttonBgColor);
		vsCodeVarMap.put("vscode-button-foreground", buttonFgColor);
		vsCodeVarMap.put("vscode-button-hoverBackground", buttonHoverBgColor);
		vsCodeVarMap.put("vscode-button-secondaryBackground", buttonSecondaryBgColor);
		vsCodeVarMap.put("vscode-button-secondaryForeground", textColor);
		vsCodeVarMap.put("vscode-button-secondaryHoverBackground", buttonSecondaryHoverBgColor);
		vsCodeVarMap.put("vscode-focusBorder", focusColor);
		vsCodeVarMap.put("vscode-list-hoverBackground", listHoverBg);
		vsCodeVarMap.put("vscode-editor-inactiveSelectionBackground", inactiveSelectionBg);
		vsCodeVarMap.put("vscode-checkbox-background", inputBgColor);
		vsCodeVarMap.put("vscode-checkbox-foreground", textColor);
		vsCodeVarMap.put("vscode-checkbox-selectBackground", buttonBgColor);
		vsCodeVarMap.put("vscode-checkbox-border", borderColor);
		vsCodeVarMap.put("vscode-badge-background", buttonBgColor);
		vsCodeVarMap.put("vscode-badge-foreground", buttonFgColor);
		vsCodeVarMap.put("vscode-scrollbarSlider-background", inputBgColor);
		vsCodeVarMap.put("vscode-scrollbarSlider-hoverBackground", adjustForHover(inputBgColor, dark));
		vsCodeVarMap.put("vscode-scrollbarSlider-activeBackground", adjustBrightness(inputBgColor, dark ? 1.2f : 0.8f));
		htmlStyled = replaceRemainingCssVars(htmlStyled, vsCodeVarMap);

		htmlStyled = htmlStyled.replace("${headerEnd}", "");
		htmlStyled = htmlStyled.replace("${nonce}", nonce);
		htmlStyled = htmlStyled.replace("nonce=\"ideNonce\"", "nonce=\"" + nonce + "\"");
		htmlStyled = htmlStyled.replace("'nonce-ideNonce'", "'nonce-" + nonce + "'");
		htmlStyled = htmlStyled.replace("nonce=ideNonce", "nonce=" + nonce);
		htmlStyled = htmlStyled.replace("${ideScript}", "");

		return htmlStyled;
	}

	private String replaceRemainingCssVars(String html, Map<String, String> varMap) {
		Matcher m = CSS_VAR_PATTERN.matcher(html);
		StringBuilder sb = new StringBuilder();
		while (m.find()) {
			String varName = m.group(1);
			String replacement = varMap.get(varName);
			m.appendReplacement(sb, Matcher.quoteReplacement(replacement != null ? replacement : m.group(0)));
		}
		m.appendTail(sb);
		return sb.toString();
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
		return String.format(Locale.ROOT, "%.3frem", targetSizePt / startingFontSizePt);
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

	// Clears per-instance color cache when Eclipse theme changes at runtime.
	// Guards against IllegalStateException in headless/test mode where PlatformUI is unavailable.
	@SuppressWarnings("PMD.NullAssignment")
	private void invalidateCacheOnThemeChange() {
		try {
			ITheme activeTheme = PlatformUI.getWorkbench().getThemeManager().getCurrentTheme();
			if (currentTheme != null && !currentTheme.equals(activeTheme)) {
				colorCache.clear();
				colorRegistry = null;
				currentTheme = null;
			}
		} catch (IllegalStateException e) {
			SnykLogger.logInfo("PlatformUI not available for theme-change check (headless/test mode)");
		}
	}

	/**
	 * Adjusts a color for a hover/active state in a theme-aware direction.
	 * Light surfaces darken on hover (factor < 1); dark surfaces brighten (factor > 1).
	 */
	private String adjustForHover(String hexColor, boolean dark) {
		return adjustBrightness(hexColor, dark ? 1.15f : 0.88f);
	}

	/**
	 * Returns white or black foreground depending on the perceived luminance of the background.
	 * Ensures button text meets WCAG AA contrast regardless of the resolved button background.
	 */
	private String pickReadableForeground(String bgHex) {
		if (bgHex == null || bgHex.length() < 7 || !bgHex.startsWith("#")) {
			return DEFAULT_WHITE_COLOR;
		}
		try {
			int r = Integer.parseInt(bgHex.substring(1, 3), 16);
			int g = Integer.parseInt(bgHex.substring(3, 5), 16);
			int b = Integer.parseInt(bgHex.substring(5, 7), 16);
			// Relative luminance per WCAG 2.1 (sRGB linearisation simplified to standard coefficients)
			double luminance = (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255.0;
			return luminance > 0.5 ? "#000000" : DEFAULT_WHITE_COLOR;
		} catch (NumberFormatException | IndexOutOfBoundsException e) {
			return DEFAULT_WHITE_COLOR;
		}
	}

	/**
	 * Returns true when Eclipse's dark-background color key is present in the color registry.
	 * THEME_DARK_BACKGROUND is only registered by Eclipse's dark theme variants; its absence
	 * signals a light theme. In test mode getColorAsHex always returns "" so this returns false.
	 * NOTE: return type must stay Boolean (not boolean) — changing the descriptor is a binary
	 * incompatible change in Eclipse's OSGi/JDT incremental class loader.
	 */
	public Boolean isDarkTheme() {
		var darkColor = getColorAsHex(THEME_DARK_BACKGROUND, "");
		return !darkColor.isEmpty();
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
