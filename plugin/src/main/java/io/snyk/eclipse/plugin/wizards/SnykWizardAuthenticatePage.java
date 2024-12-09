package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class SnykWizardAuthenticatePage extends WizardPage implements Listener {
	private Text endpoint;
	private Button unknownCerts;
	private String trustMessage = "⚠️ When scanning folder files, Snyk may automatically execute code such as invoking the package manager to get dependency information. "
			+ "You should only scan projects you trust. <a href=\"https://docs.snyk.io/ide-tools/eclipse-plugin/folder-trust\">More Info</a>"
			+ "\n\nOn finishing the wizard, the plugin will open a browser to authenticate you, trust the current workspace projects and trigger a scan.";
	private Color blackColor;

	public SnykWizardAuthenticatePage() {
		super("Snyk Wizard");
		setTitle("Authenticate");
		setDescription(
				"Review the endpoint configuration, clicking 'Finish' will authenticate with Snyk; this will open a new browser window.");
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);

		GridLayout gl = new GridLayout();
		int ncol = 2;
		gl.numColumns = ncol;
		composite.setLayout(gl);

		Group endpointGroup = SWTWidgetHelper.createGroup(composite, "");

		Label endpointLabel = new Label(endpointGroup, SWT.NONE);
		endpointLabel.setText("Endpoint:");
		endpoint = new Text(endpointGroup, SWT.BORDER | SWT.READ_ONLY);
		endpoint.setLayoutData(gd);

		Label unknownCertsLabel = new Label(endpointGroup, SWT.NONE);
		unknownCertsLabel.setText("Allow unknown certificate authorities:");

		unknownCerts = new Button(endpointGroup, SWT.CHECK);
		unknownCerts.setLayoutData(gd);

		Group trustGroup = SWTWidgetHelper.createGroup(composite, "");

		Link trustText = new Link(trustGroup, SWT.NONE);
		trustText.setText(trustMessage);
		gd = new GridData(GridData.FILL_BOTH);
		trustText.setLayoutData(gd);
		this.blackColor = new Color(0, 0, 0, 0);
		trustText.setBackground(blackColor);
		trustText.addListener(SWT.Selection, event -> org.eclipse.swt.program.Program.launch(event.text));

		// required to avoid an error in the system
		setControl(composite);
		setPageComplete(false);
	}

	@Override
	public void dispose() {
		this.blackColor.dispose();
		this.blackColor = null;
		super.dispose();
	}

	public void handleEvent(Event e) {
		getWizard().getContainer().updateButtons();
	}

	public boolean isPageComplete() {
		return true;
	}

	void onEnterPage() {
		endpoint.setText(Preferences.getInstance().getEndpoint());
		unknownCerts.setSelection(Preferences.getInstance().getBooleanPref(Preferences.INSECURE_KEY));
	}
}
