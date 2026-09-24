package io.snyk.eclipse.plugin.html;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.preferences.InMemorySecurePreferenceStore;
import io.snyk.eclipse.plugin.preferences.Preferences;

class TreeViewHtmlProviderTest {

	@BeforeEach
	void setUp() {
		// Test mode makes getColorAsHex return "" so replaceCssVariables resolves without a workbench.
		Preferences prefs = Preferences.getTestInstance(new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		prefs.setTest(true);
	}

	@Test
	void getCss_overridesFeedbackBannerLinkColor() {
		TreeViewHtmlProvider provider = new TreeViewHtmlProvider();

		assertTrue(provider.getCss().contains(".feedback-banner-link"),
				"tree provider must override the feedback-banner link color");
	}

	// The override reaches the rendered HTML only if it has an ${ideStyle} injection point. This locks in
	// the plugin-side contract so a getCss()/injection regression is caught. (The live LS tree template
	// contains ${ideStyle} — verified in snyk-ls treeview/template/tree.html — but that is served at
	// runtime and cannot be asserted from a plugin unit test.)
	@Test
	void replaceCssVariables_injectsFeedbackBannerLinkRule_whenIdeStyleMarkerPresent() {
		TreeViewHtmlProvider provider = new TreeViewHtmlProvider();
		String html = "<html><head><style>{{.Styles}}</style>${ideStyle}</head><body></body></html>";

		String result = provider.replaceCssVariables(html);

		assertTrue(result.contains(".feedback-banner-link"),
				"the getCss override must be injected at the ${ideStyle} marker");
		assertFalse(result.contains("${ideStyle}"), "the ${ideStyle} marker must be consumed");
	}

	@Test
	void replaceCssVariables_omitsRule_whenNoInjectionPoint() {
		TreeViewHtmlProvider provider = new TreeViewHtmlProvider();
		String html = "<html><head></head><body></body></html>";

		String result = provider.replaceCssVariables(html);

		assertFalse(result.contains(".feedback-banner-link"),
				"without an ${ideStyle} injection point the override cannot appear");
	}
}
