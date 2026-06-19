package io.snyk.eclipse.plugin.wizards;

import io.snyk.eclipse.plugin.preferences.AuthConstants;
import io.snyk.eclipse.plugin.preferences.Preferences;
import java.util.regex.Pattern;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Text;

public class SnykWizardAuthenticatePage extends WizardPage {

    private static final Pattern ENDPOINT_PATTERN =
            Pattern.compile("^https://api\\.([^/]*\\.)?snyk(gov)?\\.io$");

    private Combo authMethodCombo;
    private Text endpointText;
    private Button insecureCheck;

    public SnykWizardAuthenticatePage() {
        super("Snyk Wizard");
    }

    @Override
    public void createControl(Composite parent) {
        setTitle("Welcome to Snyk for Eclipse!");
        setDescription("Configure authentication settings, then click 'Finish' to authenticate with Snyk.");
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(2, false));

        // Auth method
        Label authMethodLabel = new Label(composite, SWT.NONE);
        authMethodLabel.setText("Authentication method:");
        authMethodLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));

        authMethodCombo = new Combo(composite, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (AuthConstants.AuthOptionData option : AuthConstants.OPTION_DEFINITIONS) {
            authMethodCombo.add(option.displayName());
        }
        authMethodCombo.select(0); // default: OAuth2
        authMethodCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

        // API endpoint
        Label endpointLabel = new Label(composite, SWT.NONE);
        endpointLabel.setText("Snyk API endpoint:");
        endpointLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));

        String initialEndpoint = Preferences.getInstance().getEndpoint();
        String endpointValue = (initialEndpoint == null || initialEndpoint.isBlank())
                ? Preferences.DEFAULT_ENDPOINT
                : initialEndpoint;

        endpointText = new Text(composite, SWT.BORDER);
        endpointText.setText(endpointValue);
        endpointText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        endpointText.addModifyListener(e -> validateEndpoint());

        // Insecure checkbox (spans both columns)
        new Label(composite, SWT.NONE); // filler
        insecureCheck = new Button(composite, SWT.CHECK);
        insecureCheck.setText("Disable certificate checks (insecure)");
        insecureCheck.setSelection(Preferences.getInstance().isInsecure());
        insecureCheck.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

        new Label(composite, SWT.SEPARATOR | SWT.HORIZONTAL)
                .setLayoutData(newHSpan2(SWT.FILL, SWT.CENTER, true, false));

        Label steps = new Label(composite, SWT.NONE);
        steps.setText(
                "1. Authenticate to Snyk.io\n"
                + "2. Analyze code for issues and vulnerabilities\n"
                + "3. Improve your code and upgrade dependencies");
        steps.setLayoutData(newHSpan2(SWT.FILL, SWT.TOP, true, false));

        new Label(composite, SWT.SEPARATOR | SWT.HORIZONTAL)
                .setLayoutData(newHSpan2(SWT.FILL, SWT.CENTER, true, false));

        Link trustText = new Link(composite, SWT.WRAP);
        trustText.setText(
                "When scanning project files, Snyk may automatically execute code "
                + "such as invoking the package manager to get dependency information. "
                + "You should only scan projects you trust. "
                + "<a href=\"https://docs.snyk.io/ide-tools/eclipse-plugin/folder-trust\">More info</a>");
        GridData trustData = newHSpan2(SWT.FILL, SWT.FILL, true, true);
        trustData.widthHint = 400;
        trustText.setLayoutData(trustData);
        trustText.addListener(SWT.Selection, event -> org.eclipse.swt.program.Program.launch(event.text));

        new Label(composite, SWT.SEPARATOR | SWT.HORIZONTAL)
                .setLayoutData(newHSpan2(SWT.FILL, SWT.CENTER, true, false));

        Link policyText = new Link(composite, SWT.WRAP);
        policyText.setText(
                "By connecting your account with Snyk, you agree to the "
                + "Snyk <a href=\"https://snyk.io/policies/privacy/\">Privacy Policy</a> and the "
                + "Snyk <a href=\"https://snyk.io/policies/terms-of-service/\">Terms of Service</a>.");
        policyText.setLayoutData(newHSpan2(SWT.FILL, SWT.BOTTOM, true, false));
        policyText.addListener(SWT.Selection, event -> org.eclipse.swt.program.Program.launch(event.text));

        setControl(composite);
        validateEndpoint();
    }

    private void validateEndpoint() {
        String v = endpointText.getText().trim();
        if (v.isBlank() || ENDPOINT_PATTERN.matcher(v).matches()) {
            setErrorMessage(null);
            setPageComplete(true);
        } else {
            setErrorMessage("Must be a Snyk API URL, e.g. https://api.snyk.io or https://api.eu.snyk.io");
            setPageComplete(false);
        }
    }

    private static GridData newHSpan2(int hAlign, int vAlign, boolean hGrab, boolean vGrab) {
        GridData gd = new GridData(hAlign, vAlign, hGrab, vGrab);
        gd.horizontalSpan = 2;
        return gd;
    }

    public String getAuthMethod() {
        int idx = authMethodCombo.getSelectionIndex();
        if (idx < 0 || idx >= AuthConstants.OPTION_DEFINITIONS.size()) {
            return AuthConstants.AUTH_OAUTH2;
        }
        return AuthConstants.OPTION_DEFINITIONS.get(idx).value();
    }

    public String getEndpoint() {
        String text = endpointText.getText().trim();
        return text.isBlank() ? Preferences.DEFAULT_ENDPOINT : text;
    }

    public boolean isInsecure() {
        return insecureCheck.getSelection();
    }

}
