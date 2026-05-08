package io.snyk.eclipse.plugin.html;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;

public class EclipseThemeCssProvider {

	static String toHex(Color color) {
		return String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
	}

	static String toRgba(Color color, double alpha) {
		return String.format("rgba(%d,%d,%d,%.2f)", color.getRed(), color.getGreen(), color.getBlue(), alpha)
				.replaceAll(",0+(\\d)", ",$1");
	}

	static String quoteFontFamily(String name) {
		if (name.contains(" ")) {
			return "\"" + name.replace("\"", "") + "\"";
		}
		return name;
	}

	public static String buildStyleBlock(Display display) {
		String foreground = resolveColor(display, SWT.COLOR_LIST_FOREGROUND, "#000000");
		String descriptionFg = resolveColor(display, SWT.COLOR_WIDGET_FOREGROUND, "#444444");
		String errorFg = resolveColor(display, SWT.COLOR_RED, "#f44747");
		String sidebarBg = resolveColor(display, SWT.COLOR_LIST_BACKGROUND, "#ffffff");
		String panelBorder = resolveColor(display, SWT.COLOR_WIDGET_BORDER, "#cccccc");
		String widgetShadow = resolveColorRgba(display, SWT.COLOR_WIDGET_DARK_SHADOW, 0.15, "rgba(0,0,0,0.15)");
		String focusBorder = resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc");
		String listHoverBg = resolveColorRgba(display, SWT.COLOR_LIST_SELECTION, 0.12, "rgba(0,102,204,0.12)");
		String listSelectionBg = resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc");
		String listSelectionFg = resolveColor(display, SWT.COLOR_LIST_SELECTION_TEXT, "#ffffff");
		String inputBg = resolveColor(display, SWT.COLOR_WIDGET_BACKGROUND, "#f3f3f3");
		String dropdownBg = resolveColor(display, SWT.COLOR_WIDGET_BACKGROUND, "#f3f3f3");
		String buttonBg = resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc");
		FontData fontData = resolveFontData(display);
		String fontFamily = fontData != null ? quoteFontFamily(fontData.getName()) : "system-ui,sans-serif";
		String fontSize = fontData != null ? fontData.getHeight() + "pt" : "13pt";

		return "<style>:root {"
				+ "--vscode-font-family:" + fontFamily + ";"
				+ "--vscode-font-size:" + fontSize + ";"
				+ "--vscode-foreground:" + foreground + ";"
				+ "--vscode-descriptionForeground:" + descriptionFg + ";"
				+ "--vscode-errorForeground:" + errorFg + ";"
				+ "--vscode-sideBar-background:" + sidebarBg + ";"
				+ "--vscode-panel-border:" + panelBorder + ";"
				+ "--vscode-widget-shadow:" + widgetShadow + ";"
				+ "--vscode-focusBorder:" + focusBorder + ";"
				+ "--vscode-list-hoverBackground:" + listHoverBg + ";"
				+ "--vscode-list-activeSelectionBackground:" + listSelectionBg + ";"
				+ "--vscode-list-activeSelectionForeground:" + listSelectionFg + ";"
				+ "--vscode-input-background:" + inputBg + ";"
				+ "--vscode-dropdown-background:" + dropdownBg + ";"
				+ "--vscode-button-background:" + buttonBg + ";"
				+ "}</style>";
	}

	private static String resolveColor(Display display, int swtColorConstant, String fallback) {
		if (display == null || display.isDisposed()) {
			return fallback;
		}
		Color color = display.getSystemColor(swtColorConstant);
		return color != null ? toHex(color) : fallback;
	}

	private static String resolveColorRgba(Display display, int swtColorConstant, double alpha, String fallback) {
		if (display == null || display.isDisposed()) {
			return fallback;
		}
		Color color = display.getSystemColor(swtColorConstant);
		return color != null ? toRgba(color, alpha) : fallback;
	}

	private static FontData resolveFontData(Display display) {
		if (display == null || display.isDisposed()) {
			return null;
		}
		try {
			FontData[] data = display.getSystemFont().getFontData();
			return (data != null && data.length > 0) ? data[0] : null;
		} catch (Exception e) {
			return null;
		}
	}
}
