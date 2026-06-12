package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;

public class SnykWizardAuthenticatePage extends WizardPage {

	public SnykWizardAuthenticatePage() {
		super("Snyk Wizard");
	}

	@Override
	public void createControl(Composite parent) {
		setTitle("Welcome to Snyk for Eclipse!");
		setDescription("Clicking 'Finish' will open a browser to authenticate with Snyk.");
		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new GridLayout(1, false));

		Label steps = new Label(composite, SWT.NONE);
		steps.setText(
				"1. Authenticate to Snyk.io\n"
				+ "2. Analyze code for issues and vulnerabilities\n"
				+ "3. Improve your code and upgrade dependencies");
		steps.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		new Label(composite, SWT.SEPARATOR | SWT.HORIZONTAL)
				.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		Link trustText = new Link(composite, SWT.WRAP);
		trustText.setText(
				"When scanning project files, Snyk may automatically execute code "
				+ "such as invoking the package manager to get dependency information. "
				+ "You should only scan projects you trust. "
				+ "<a href=\"https://docs.snyk.io/ide-tools/eclipse-plugin/folder-trust\">More info</a>");
		GridData trustData = new GridData(SWT.FILL, SWT.FILL, true, true);
		trustData.widthHint = 400;
		trustText.setLayoutData(trustData);
		trustText.addListener(SWT.Selection, event -> org.eclipse.swt.program.Program.launch(event.text));

		new Label(composite, SWT.SEPARATOR | SWT.HORIZONTAL)
				.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		Link policyText = new Link(composite, SWT.WRAP);
		policyText.setText(
				"By connecting your account with Snyk, you agree to the "
				+ "Snyk <a href=\"https://snyk.io/policies/privacy/\">Privacy Policy</a> and the "
				+ "Snyk <a href=\"https://snyk.io/policies/terms-of-service/\">Terms of Service</a>.");
		policyText.setLayoutData(new GridData(SWT.FILL, SWT.BOTTOM, true, false));
		policyText.addListener(SWT.Selection, event -> org.eclipse.swt.program.Program.launch(event.text));

		setControl(composite);
		setPageComplete(true);
	}

	@Override
	public boolean isPageComplete() {
		return true;
	}
}
