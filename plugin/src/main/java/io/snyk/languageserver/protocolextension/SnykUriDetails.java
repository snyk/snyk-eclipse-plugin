package io.snyk.languageserver.protocolextension;

import java.net.URI;
import static io.snyk.eclipse.plugin.domain.ProductConstants.DIAGNOSTIC_SOURCE_SNYK_CODE;

public record SnykUriDetails(String scheme, String filePath, String product,
		String action, String issueId) {
	public static SnykUriDetails fromURI(URI uri) {
		return new SnykUriDetails(uri.getScheme(), uri.getPath(),
				SnykUriUtils.getDecodedParam(uri, "product"),
				SnykUriUtils.getDecodedParam(uri, "action"),
				SnykUriUtils.getDecodedParam(uri, "issueId"));
	}

	public boolean isValid() {
		return ("snyk".equals(this.scheme())
				&& DIAGNOSTIC_SOURCE_SNYK_CODE.equals(this.product())
				&& "showInDetailPanel".equals(this.action()));
	}
}
