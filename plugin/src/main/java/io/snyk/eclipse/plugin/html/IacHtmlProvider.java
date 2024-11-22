package io.snyk.eclipse.plugin.html;

public class IacHtmlProvider extends BaseHtmlProvider {
    private static IacHtmlProvider instance = new IacHtmlProvider();
	public static IacHtmlProvider getInstance() {
		if (instance == null) {
			synchronized (IacHtmlProvider.class) {
				if (instance == null) {
					instance = new IacHtmlProvider();
				}
			}
		}
		return instance;
	}
}