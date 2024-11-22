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
    @Override
    public String replaceCssVariables(String html) {
        html = super.replaceCssVariables(html);
        html = html.replace("var(--container-background-color)", super.getColorAsHex("org.eclipse.ui.workbench.CODE_BACKGROUND_COLOR", "#F0F0F0"));

        return html;
    }
}
