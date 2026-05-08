package io.snyk.eclipse.plugin.html;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
}
