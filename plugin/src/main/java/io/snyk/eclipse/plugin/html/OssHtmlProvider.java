package io.snyk.eclipse.plugin.html;

public class OssHtmlProvider extends BaseHtmlProvider {
    private static OssHtmlProvider instance = new OssHtmlProvider();

    public static OssHtmlProvider getInstance() {
        synchronized (OssHtmlProvider.class) {
            if (instance == null) {
                instance = new OssHtmlProvider();
            }
        }
        return instance;
    }

    @Override
    public String replaceCssVariables(String html) {
        var returnValue = super.replaceCssVariables(html);
        return returnValue.replace("var(--container-background-color)",
            super.getColorAsHex("org.eclipse.ui.workbench.DARK_BACKGROUND", "#F0F0F0"));
    }
}
