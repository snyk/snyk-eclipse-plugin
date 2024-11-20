package io.snyk.eclipse.plugin.html;

public class OssHtmlProvider extends BaseHtmlProvider {
    private static OssHtmlProvider instance = new OssHtmlProvider();
	public static OssHtmlProvider getInstance() {
		if (instance == null) {
			synchronized (OssHtmlProvider.class) {
				if (instance == null) {
					instance = new OssHtmlProvider();
				}
			}
		}
		return instance;
	}
}
