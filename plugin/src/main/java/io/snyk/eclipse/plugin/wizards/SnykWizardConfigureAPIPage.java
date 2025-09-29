package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;

import io.snyk.eclipse.plugin.preferences.Preferences;

public class SnykWizardConfigureAPIPage extends WizardPage implements Listener {
	private Text endpoint;
	private Button unknownCerts;
	private String initialEndpoint = Preferences.getInstance().getEndpoint();

	public SnykWizardConfigureAPIPage() {
		super("Snyk Wizard");
		this.setTitle("Configure Snyk API");
		this.setDescription("Before scanning your code for vulnerabilities, we must first authenticate with Snyk.");
	}

	@Override
	public final void setTitle(String title) {
		super.setTitle(title);
	}

	@Override
	public final void setDescription(String description) {
		super.setDescription(description);
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridData gd;

		GridLayout gl = new GridLayout();
		int ncol = 2;
		gl.numColumns = ncol;
		composite.setLayout(gl);

		Label endpointLabel = new Label(composite, SWT.NONE);
		endpointLabel.setText(
				"Specify the Snyk API endpoint. Useful for custom Multi Tenant or Single Tenant setup (default: "
						+ Preferences.DEFAULT_ENDPOINT + ":");

		endpoint = new Text(composite, SWT.BORDER);

		gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalSpan = ncol;
		endpoint.setLayoutData(gd);

		String endpointValue = initialEndpoint == null || initialEndpoint.isBlank() ? Preferences.DEFAULT_ENDPOINT
				: initialEndpoint;
		endpoint.setText(endpointValue);

		createLine(composite, ncol);

		Label unknownCertsLabel = new Label(composite, SWT.NONE);
		unknownCertsLabel.setText("Disable certificate checks for SSL connections:");

		unknownCerts = new Button(composite, SWT.CHECK);
		unknownCerts.setLayoutData(gd);

		unknownCerts.setSelection(Preferences.getInstance().isInsecure());

		// required to avoid an error in the system
		setControl(composite);
		setPageComplete(false);
	}

	@Override
	public void handleEvent(Event e) {
		getWizard().getContainer().updateButtons();
	}

	@Override
	public boolean canFlipToNextPage() {
		return true;
	}

	@Override
	public IWizardPage getNextPage() {
		SnykWizardAuthenticatePage page = ((SnykWizard) getWizard()).authenticatePage;
		page.setEndpoint(endpoint.getText());
		page.setUnknownCerts(unknownCerts.getSelection());

		return page;
	}

	private void createLine(Composite parent, int ncol) {
		Label line = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.BOLD);
		GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		gridData.horizontalSpan = ncol;
		line.setLayoutData(gridData);
	}
}
