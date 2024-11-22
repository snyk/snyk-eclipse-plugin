package io.snyk.eclipse.plugin.html;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;

import io.snyk.eclipse.plugin.domain.ProductConstants;
import io.snyk.languageserver.LsBaseTest;

public class HtmlProviderFactoryTest {
  @Test
  void htmlProviderFactoryReturnsCorrectType() throws Exception {
	var cut = HtmlProviderFactory.GetHtmlProvider(ProductConstants.DISPLAYED_CODE_SECURITY);
	assertTrue(cut instanceof CodeHtmlProvider);
	cut = HtmlProviderFactory.GetHtmlProvider(ProductConstants.DISPLAYED_CODE_QUALITY);
	assertTrue(cut instanceof CodeHtmlProvider);
	cut = HtmlProviderFactory.GetHtmlProvider(ProductConstants.DISPLAYED_OSS);
	assertTrue(cut instanceof OssHtmlProvider);
	cut = HtmlProviderFactory.GetHtmlProvider(ProductConstants.DISPLAYED_IAC);
	assertTrue(cut instanceof IacHtmlProvider);
  }
  
  @Test
  void testHtmlProviderReplacesPlaceholders() throws Exception {
	var htmlContent = Files.readString(Path.of("src/test/resources/code_issue_description.html"));

	var cut = HtmlProviderFactory.GetHtmlProvider(ProductConstants.DISPLAYED_CODE_SECURITY);
	htmlContent = cut.replaceCssVariables(htmlContent);
    assertTrue(!htmlContent.contains("${headerEnd})"));
    assertTrue(!htmlContent.contains("${nonce})"));
    assertTrue(!htmlContent.contains("${ideScript})"));
    assertTrue(!htmlContent.contains("ideNonce"));
    assertTrue(!htmlContent.contains("var(--text-color)"));
    assertTrue(!htmlContent.contains("var(----background-color)"));
    assertTrue(!htmlContent.contains("var(--border-color)"));
    assertTrue(!htmlContent.contains("var(--link-color)"));
    assertTrue(!htmlContent.contains("var(--text-color)"));
    assertTrue(!htmlContent.contains("var(--horizontal-border-color)"));
    assertTrue(!htmlContent.contains("var(--code-background-color)"));
    assertTrue(!htmlContent.contains("var(--example-line-removed-color)"));
    assertTrue(!htmlContent.contains("var(--example-line-added-color)"));
  }
  
  @Test
  void testHtmlProviderGeneratesInitScript() throws Exception {
	var cut = HtmlProviderFactory.GetHtmlProvider(ProductConstants.DISPLAYED_CODE_SECURITY);
	var initScript = cut.getInitScript();
    assertTrue(initScript.contains("window.openInEditor"));
    assertTrue(initScript.contains("document.body.classList.add(isHighContrast"));
  }
}
