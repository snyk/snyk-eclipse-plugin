package io.snyk.eclipse.plugin.html;

import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;

public class EclipseThemeCssProvider {

	static String toHex(Color color) {
		return String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
	}

	static String toRgba(Color color, double alpha) {
		return String.format(Locale.ROOT, "rgba(%d,%d,%d,%.2f)", color.getRed(), color.getGreen(), color.getBlue(), alpha);
	}

	static String quoteFontFamily(String name) {
		if (name.contains(" ")) {
			return "\"" + name.replace("\"", "") + "\"";
		}
		return name;
	}

	public static Map<String, String> buildVariableMap(Display display) {
		FontData fontData = resolveFontData(display);
		String fontFamily = fontData != null
				? quoteFontFamily(fontData.getName()) + ", system-ui, -apple-system, sans-serif"
				: "system-ui, -apple-system, sans-serif";
		String fontSize = fontData != null ? fontData.getHeight() + "px" : "13px";

		Map<String, String> vars = new LinkedHashMap<>();
		vars.put("--vscode-font-family", fontFamily);
		vars.put("--vscode-font-size", fontSize);
		vars.put("--vscode-editor-font-family", "monospace");
		vars.put("--vscode-editor-font-size", "12px");
		vars.put("--vscode-foreground", resolveColor(display, SWT.COLOR_LIST_FOREGROUND, "#000000"));
		vars.put("--vscode-descriptionForeground", resolveColor(display, SWT.COLOR_WIDGET_FOREGROUND, "#888888"));
		vars.put("--vscode-errorForeground", resolveColor(display, SWT.COLOR_RED, "#f44747"));
		vars.put("--vscode-sideBar-background", resolveColor(display, SWT.COLOR_LIST_BACKGROUND, "#ffffff"));
		vars.put("--vscode-panel-border", resolveColor(display, SWT.COLOR_WIDGET_BORDER, "#cccccc"));
		vars.put("--vscode-widget-shadow", resolveColorRgba(display, SWT.COLOR_WIDGET_DARK_SHADOW, 0.15, "rgba(0,0,0,0.15)"));
		vars.put("--vscode-focusBorder", resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc"));
		vars.put("--vscode-list-hoverBackground", resolveColorRgba(display, SWT.COLOR_LIST_SELECTION, 0.12, "rgba(0,102,204,0.12)"));
		vars.put("--vscode-list-activeSelectionBackground", resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc"));
		vars.put("--vscode-list-activeSelectionForeground", resolveColor(display, SWT.COLOR_LIST_SELECTION_TEXT, "#ffffff"));
		vars.put("--vscode-tree-indentGuidesStroke", resolveColorRgba(display, SWT.COLOR_WIDGET_BORDER, 0.60, "rgba(128,128,128,0.60)"));
		vars.put("--vscode-input-background", resolveColor(display, SWT.COLOR_WIDGET_BACKGROUND, "#f3f3f3"));
		vars.put("--vscode-input-foreground", resolveColor(display, SWT.COLOR_LIST_FOREGROUND, "#000000"));
		vars.put("--vscode-input-border", resolveColor(display, SWT.COLOR_WIDGET_BORDER, "#cccccc"));
		vars.put("--vscode-dropdown-background", resolveColor(display, SWT.COLOR_WIDGET_BACKGROUND, "#f3f3f3"));
		vars.put("--vscode-dropdown-border", resolveColor(display, SWT.COLOR_WIDGET_BORDER, "#cccccc"));
		vars.put("--vscode-dropdown-foreground", resolveColor(display, SWT.COLOR_LIST_FOREGROUND, "#000000"));
		vars.put("--vscode-button-background", resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0066cc"));
		vars.put("--vscode-button-foreground", resolveColor(display, SWT.COLOR_LIST_SELECTION_TEXT, "#ffffff"));
		vars.put("--vscode-button-hoverBackground", resolveColor(display, SWT.COLOR_LIST_SELECTION, "#0055aa"));
		vars.put("--vscode-activityWarningBadge-background", "#CCA700");
		vars.put("--vscode-activityWarningBadge-foreground", "#15202B");
		vars.put("--vscode-inputValidation-infoBackground", "#e6f3ff");
		return vars;
	}

	public static String buildStyleBlock(Display display) {
		Map<String, String> vars = buildVariableMap(display);
		StringBuilder sb = new StringBuilder("<style>:root {");
		for (Map.Entry<String, String> entry : vars.entrySet()) {
			sb.append(entry.getKey()).append(':').append(entry.getValue()).append(';');
		}
		sb.append("}</style>");
		return sb.toString();
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
		FontData[] data = display.getSystemFont().getFontData();
		return (data != null && data.length > 0) ? data[0] : null;
	}
}
