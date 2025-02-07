package io.snyk.eclipse.plugin.html;

import io.snyk.eclipse.plugin.domain.ProductConstants;

public class HtmlProviderFactory {

	public static BaseHtmlProvider GetHtmlProvider(String product) {
		switch (product) {
		case ProductConstants.DISPLAYED_CODE_SECURITY:
		case ProductConstants.DISPLAYED_CODE_QUALITY:
			return CodeHtmlProvider.getInstance();
		case ProductConstants.DISPLAYED_OSS:
			return OssHtmlProvider.getInstance();
		case ProductConstants.DISPLAYED_IAC:
			return IacHtmlProvider.getInstance();
		default:
			return null;
		}
	}
}
