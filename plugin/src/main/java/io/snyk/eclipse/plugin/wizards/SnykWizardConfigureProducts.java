package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

public class SnykWizardConfigureProducts extends WizardPage {
	Button openSourceEnabled;
	Button codeEnabled;
	Button iacEnabled;
	private Composite container;

	public SnykWizardConfigureProducts() {
        super("Snyk Wizard");
        setTitle("Configure Snyk Products");
        setDescription("The following options involves the Snyk Language Server. Activating Snyk Code will cause upload of source code to Snyk or the given endpoint address.");
    }
 
	@Override
    public void createControl(Composite parent) {
        container = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        container.setLayout(layout);

        new Label(container, SWT.NONE).setText("Snyk Open Source enabled: Enable/Disable Snyk Open Source Dependency Scans via Language Server.");
        openSourceEnabled = new Button(container, SWT.CHECK);
        openSourceEnabled.setSelection(true);
        this.setOpenSourceEnabled(openSourceEnabled);
        
        new Label(container, SWT.NONE).setText("Snyk Code enabled: Enable/Disable Snyk Code Scans via Language Server.");
        codeEnabled = new Button(container, SWT.CHECK);
        codeEnabled.setSelection(false);
        this.setCodeEnabled(codeEnabled);
        
        new Label(container, SWT.NONE).setText("Snyk Infrastructure-as-Code enabled : Enable/Disable Snyk IaC Scans via Language Server.");
        iacEnabled = new Button(container, SWT.CHECK);
        iacEnabled.setSelection(false);
        this.setIacEnabled(iacEnabled);
        
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        openSourceEnabled.setLayoutData(gd);
        codeEnabled.setLayoutData(gd);
        iacEnabled.setLayoutData(gd);
        
        // required to avoid an error in the system
        setControl(container);
        setPageComplete(false);
    }
	  
	private void setOpenSourceEnabled(Button openSourceEnabled) {
	  // TODO set preferences
	}
	
	private void setCodeEnabled(Button openCodeEnabled) {
	  // TODO set preferences
	}
	
	private void setIacEnabled(Button iacEnabled) {
	  // TODO set preferences
	}

	public Boolean getOpenSourceEnabled() {
	  return openSourceEnabled.getSelection();
	}
	
	public Boolean getCodeEnabled() {
	  return codeEnabled.getSelection();
	}
	
	public Boolean getiacEnabled() {
	  return iacEnabled.getSelection();
	}
}
