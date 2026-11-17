package io.snyk.eclipse.plugin.html;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.preferences.InMemoryPreferenceStore;
import io.snyk.eclipse.plugin.preferences.InMemorySecurePreferenceStore;
import io.snyk.eclipse.plugin.preferences.Preferences;

class CodeHtmlProviderTest {

	@BeforeEach
	void setUp() {
		Preferences preferences = Preferences.getTestInstance(
				new InMemoryPreferenceStore(), new InMemorySecurePreferenceStore());
		preferences.setTest(true);
	}

	@Test
	void replaceCssVariables_removesSnykLearnContentFromIssueDescription() throws Exception {
		String html = Files.readString(Path.of("src/test/resources/code_issue_description.html"));

		String result = CodeHtmlProvider.getInstance().replaceCssVariables(html);

		assertFalse(result.contains("<div class=\"lesson-container\""));
		assertFalse(result.contains("learn.snyk.io"));
		assertFalse(result.contains("Learn about this vulnerability"));
	}

	@Test
	void replaceCssVariables_preservesUnrelatedIssueDetailsAndLinks() throws Exception {
		String html = Files.readString(Path.of("src/test/resources/code_issue_description.html"));

		String result = CodeHtmlProvider.getInstance().replaceCssVariables(html);

		assertTrue(result.contains("NoSQL Injection"));
		assertTrue(result.contains("Priority score: 818"));
		assertTrue(result.contains("https://cwe.mitre.org/data/definitions/943.html"));
		assertTrue(result.contains("https://app.snyk.io"));
		assertTrue(result.contains("https://github.com/afuh/pinstagram/commit/"));
	}

	@Test
	void replaceCssVariables_removesWholeLearnContainerWithNestedElements() {
		String html = """
				<header>
				  <div class="summary">Issue summary</div>
				  <div class="lesson-container extra-class">
				    <div class="lesson-icon"><svg><path></path></svg></div>
				    <a href="https://learn.snyk.io/lesson/example">Learn about this vulnerability</a>
				  </div>
				  <a href="https://cwe.mitre.org/data/definitions/79.html">CWE-79</a>
				</header>
				""";

		String result = CodeHtmlProvider.getInstance().replaceCssVariables(html);

		assertFalse(result.contains("<div class=\"lesson-container"));
		assertFalse(result.contains("learn.snyk.io"));
		assertTrue(result.contains("Issue summary"));
		assertTrue(result.contains("https://cwe.mitre.org/data/definitions/79.html"));
	}

	@Test
	void replaceCssVariables_removesLearnAnchorOutsideKnownContainer() {
		String html = """
				<section>
				  <a href="https://learn.snyk.io/lesson/example">Learn more</a>
				  <a href="https://docs.snyk.io/products/snyk-code">Documentation</a>
				</section>
				""";

		String result = CodeHtmlProvider.getInstance().replaceCssVariables(html);

		assertFalse(result.contains("learn.snyk.io"));
		assertFalse(result.contains("Learn more"));
		assertTrue(result.contains("https://docs.snyk.io/products/snyk-code"));
		assertTrue(result.contains("Documentation"));
	}
}
