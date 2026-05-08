package io.snyk.eclipse.plugin.html;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;

public class EclipseThemeCssProvider {

	static String toHex(Color color) {
		return String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
	}

	public static String buildStyleBlock(Display display) {
		String foreground = resolveColor(display, SWT.COLOR_LIST_FOREGROUND, "#000000");
		String descriptionFg = resolveColor(display, SWT.COLOR_WIDGET_FOREGROUND, "#444444");
		String errorFg = resolveColor(display, SWT.COLOR_RED, "#f44747");
		String sidebarBg = resolveColor(display, SWT.COLOR_LIST_BACKGROUND, "#ffffff");
		String panelBorder = resolveColor(display, SWT.COLOR_WIDGET_BORDER, "#cccccc");
		String widgetShadow = resolveColor(display, SWT.COLOR_WIDGET_DARK_SHADOW, "#00000026");
		String focusBorder = resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc");
		String listHoverBg = resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc1f");
		String listSelectionBg = resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc");
		String listSelectionFg = resolveColor(display, SWT.COLOR_LIST_SELECTION_TEXT, "#ffffff");
		String inputBg = resolveColor(display, SWT.COLOR_WIDGET_BACKGROUND, "#f3f3f3");
		String dropdownBg = resolveColor(display, SWT.COLOR_WIDGET_BACKGROUND, "#f3f3f3");
		String buttonBg = resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc");
		String fontFamily = resolveFontFamily(display);
		String fontSize = resolveFontSize(display);

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
		if (color == null) {
			return fallback;
		}
		return toHex(color);
	}

	private static String resolveFontFamily(Display display) {
		if (display == null || display.isDisposed()) {
			return "system-ui,sans-serif";
		}
		try {
			return display.getSystemFont().getFontData()[0].getName();
		} catch (Exception e) {
			return "system-ui,sans-serif";
		}
	}

	private static String resolveFontSize(Display display) {
		if (display == null || display.isDisposed()) {
			return "13px";
		}
		try {
			return display.getSystemFont().getFontData()[0].getHeight() + "px";
		} catch (Exception e) {
			return "13px";
		}
	}
}
