package io.snyk.eclipse.plugin.html;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.junit.jupiter.api.Test;

import io.snyk.languageserver.LsBaseTest;

class EclipseThemeCssProviderTest extends LsBaseTest {

	// T-U-001: toHex converts known RGB values to #rrggbb format
	@Test
	void toHex_convertsBlackToHex() {
		Color black = new Color(0, 0, 0);
		assertEquals("#000000", EclipseThemeCssProvider.toHex(black));
	}

	@Test
	void toHex_convertsWhiteToHex() {
		Color white = new Color(255, 255, 255);
		assertEquals("#ffffff", EclipseThemeCssProvider.toHex(white));
	}

	@Test
	void toHex_convertsRedToHex() {
		Color red = new Color(255, 0, 0);
		assertEquals("#ff0000", EclipseThemeCssProvider.toHex(red));
	}

	@Test
	void toHex_convertsArbitraryColorToHex() {
		Color color = new Color(16, 32, 48);
		assertEquals("#102030", EclipseThemeCssProvider.toHex(color));
	}

	// toRgba: converts color + alpha to CSS rgba() string
	@Test
	void toRgba_blackWithAlpha() {
		Color black = new Color(0, 0, 0);
		assertEquals("rgba(0,0,0,0.12)", EclipseThemeCssProvider.toRgba(black, 0.12));
	}

	@Test
	void toRgba_colorWithAlpha() {
		Color color = new Color(0, 102, 204);
		assertEquals("rgba(0,102,204,0.15)", EclipseThemeCssProvider.toRgba(color, 0.15));
	}

	// quoteFontFamily: wraps multi-word names in quotes, leaves single-word alone
	@Test
	void quoteFontFamily_multiWordName_isQuoted() {
		assertEquals("\"Segoe UI\"", EclipseThemeCssProvider.quoteFontFamily("Segoe UI"));
	}

	@Test
	void quoteFontFamily_singleWordName_isUnchanged() {
		assertEquals("Ubuntu", EclipseThemeCssProvider.quoteFontFamily("Ubuntu"));
	}

	// Font size comes from JFace theme — must end with px
	@Test
	void resolveThemeFontSize_endsWith_px() {
		String size = EclipseThemeCssProvider.resolveThemeFontSize();
		assertTrue(size.endsWith("px"), "font-size must end with px, got: " + size);
	}

	// Font family comes from JFace theme — must include system-ui fallback
	@Test
	void resolveThemeFontFamily_includesSystemUi() {
		String family = EclipseThemeCssProvider.resolveThemeFontFamily();
		assertTrue(family.contains("system-ui"), "font-family must include system-ui fallback");
		assertTrue(family.contains("-apple-system"), "font-family must include -apple-system fallback");
	}

	// Hover background fallback must use rgba (preserving transparency intent)
	@Test
	void buildStyleBlock_hoverBackground_fallbackUsesRgba() {
		String style = EclipseThemeCssProvider.buildStyleBlock(null);
		assertTrue(style.contains("--vscode-list-hoverBackground:rgba("), "hover bg must use rgba()");
	}

	// Widget shadow fallback must use rgba
	@Test
	void buildStyleBlock_widgetShadow_fallbackUsesRgba() {
		String style = EclipseThemeCssProvider.buildStyleBlock(null);
		assertTrue(style.contains("--vscode-widget-shadow:rgba("), "widget shadow must use rgba()");
	}

	// T-U-002: buildStyleBlock returns a <style> block with the required --vscode-* variables
	@Test
	void buildStyleBlock_containsForegroundVariable() {
		String style = EclipseThemeCssProvider.buildStyleBlock(null);
		assertTrue(style.contains("--vscode-foreground:"), "Style block must contain --vscode-foreground:");
	}

	@Test
	void buildStyleBlock_containsSideBarBackgroundVariable() {
		String style = EclipseThemeCssProvider.buildStyleBlock(null);
		assertTrue(style.contains("--vscode-sideBar-background:"),
				"Style block must contain --vscode-sideBar-background:");
	}

	@Test
	void buildStyleBlock_wrapsInStyleTag() {
		String style = EclipseThemeCssProvider.buildStyleBlock(null);
		assertTrue(style.startsWith("<style>"), "Style block must start with <style>");
		assertTrue(style.endsWith("</style>"), "Style block must end with </style>");
	}

	@Test
	void buildStyleBlock_containsRootSelector() {
		String style = EclipseThemeCssProvider.buildStyleBlock(null);
		assertTrue(style.contains(":root"), "Style block must contain :root selector");
	}

	// buildVariableMap: returns a map with all required --vscode-* variable keys
	@Test
	void buildVariableMap_containsAllRequiredKeys() {
		Map<String, String> map = EclipseThemeCssProvider.buildVariableMap(null);
		assertTrue(map.containsKey("--vscode-foreground"));
		assertTrue(map.containsKey("--vscode-sideBar-background"));
		assertTrue(map.containsKey("--vscode-list-hoverBackground"));
		assertTrue(map.containsKey("--vscode-tree-indentGuidesStroke"));
		assertTrue(map.containsKey("--vscode-button-foreground"));
	}

	@Test
	void buildVariableMap_foregroundFallbackIsBlack() {
		Map<String, String> map = EclipseThemeCssProvider.buildVariableMap(null);
		assertEquals("#000000", map.get("--vscode-foreground"));
	}

	@Test
	void buildVariableMap_sideBarBackgroundFallbackIsWhite() {
		Map<String, String> map = EclipseThemeCssProvider.buildVariableMap(null);
		assertEquals("#ffffff", map.get("--vscode-sideBar-background"));
	}

	@Test
	void buildVariableMap_listHoverBackgroundFallbackUsesRgba() {
		Map<String, String> map = EclipseThemeCssProvider.buildVariableMap(null);
		assertTrue(map.get("--vscode-list-hoverBackground").startsWith("rgba("));
	}

	@Test
	void buildVariableMap_treeIndentGuidesFallbackUsesRgba() {
		Map<String, String> map = EclipseThemeCssProvider.buildVariableMap(null);
		assertTrue(map.get("--vscode-tree-indentGuidesStroke").startsWith("rgba("));
	}

	@Test
	void buildVariableMap_buttonForegroundFallbackIsWhite() {
		Map<String, String> map = EclipseThemeCssProvider.buildVariableMap(null);
		assertEquals("#ffffff", map.get("--vscode-button-foreground"));
	}

	@Test
	void buildVariableMap_hardcodedWarningBadgeBackground() {
		Map<String, String> map = EclipseThemeCssProvider.buildVariableMap(null);
		assertEquals("#CCA700", map.get("--vscode-activityWarningBadge-background"));
	}

	@Test
	void buildVariableMap_fontSizeFallbackIsPx() {
		Map<String, String> map = EclipseThemeCssProvider.buildVariableMap(null);
		assertTrue(map.get("--vscode-font-size").endsWith("px"));
	}
}
