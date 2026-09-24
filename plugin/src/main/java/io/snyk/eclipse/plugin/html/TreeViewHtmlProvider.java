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
 *
 * <p>TEMPORARY / plugin-side workaround. The proper fix belongs in snyk-ls: give the feedback-banner
 * link its own CSS variable (or a readable-on-gradient value) instead of reusing the shared
 * {@code --button-text-color}, so every IDE benefits and none needs this element-scoped override.
 * Tracked as tech debt — remove this override once the LS emits a correct banner link color.
 */
public class TreeViewHtmlProvider extends BaseHtmlProvider {

	@Override
	public String getCss() {
		// TODO(tech-debt): remove once snyk-ls gives the feedback-banner link its own readable color
		// variable rather than the shared --button-text-color. See the class javadoc.
		return ".feedback-banner-link { color: var(--text-color); }";
	}
}
