package io.snyk.eclipse.plugin.html;

/**
 * HTML provider for the Snyk tree view. Adds tree-specific CSS on top of {@link BaseHtmlProvider}'s
 * theme-variable substitution.
 *
 * <p>The feedback banner link ({@code .feedback-banner-link} in the LS-served styles.css) uses
 * {@code var(--button-text-color)}, which resolves to the button foreground — black in the dark theme
 * (readable on a light button, but not on the banner's dark gradient). That variable is shared with the
 * real action buttons, so it cannot be re-substituted globally without breaking them; the fix has to be
 * element-scoped. This CSS is injected via the {@code ${ideStyle}} slot, which lands after the LS styles
 * so it wins by cascade order, and the emitted {@code var(--text-color)} is resolved by
 * {@link BaseHtmlProvider#replaceCssVariables} to the themed text color (white in dark, dark in light),
 * matching the banner's own text.
 */
public class TreeViewHtmlProvider extends BaseHtmlProvider {

	@Override
	public String getCss() {
		return ".feedback-banner-link { color: var(--text-color); }";
	}
}
