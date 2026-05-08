package io.snyk.languageserver.protocolextension;

import java.net.URI;

import io.snyk.eclipse.plugin.utils.UriUtils;

public record SnykShowFixUriDetails(String scheme, String filePath, String product,
		String action, String issueId) {
	public static SnykShowFixUriDetails fromURI(URI uri) {
		return new SnykShowFixUriDetails(uri.getScheme(), uri.getPath(),
				UriUtils.getDecodedParam(uri, "product"),
				UriUtils.getDecodedParam(uri, "action"),
				UriUtils.getDecodedParam(uri, "issueId"));
	}

	public boolean isValid() {
		return "snyk".equals(this.scheme())
				&& this.product() != null
				&& !this.product().isEmpty()
				&& "showInDetailPanel".equals(this.action());
	}
}
