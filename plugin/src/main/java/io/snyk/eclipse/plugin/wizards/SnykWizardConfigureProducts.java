package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Event;

public class SnykWizardConfigureProducts extends WizardPage implements Listener {
	Button openSourceEnabled;
	Button codeEnabled;
	Button iacEnabled;

	public SnykWizardConfigureProducts() {
        super("Snyk Wizard");
        setTitle("Configure Snyk Products");
        setDescription("The following options involves the Snyk Language Server. Activating Snyk Code will cause upload of source code to Snyk or the given endpoint address.");
    }
 
	@Override
    public void createControl(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        composite.setLayout(layout);

        Label openSourceLabel = new Label(composite, SWT.NONE);
        openSourceLabel.setText("Snyk Open Source enabled:");
        openSourceLabel.setToolTipText("Enable/Disable Snyk Open Source Dependency Scans via Language Server.");

        openSourceEnabled = new Button(composite, SWT.CHECK);
        openSourceEnabled.setSelection(true);
        
        Label codeLabel = new Label(composite, SWT.NONE);
        codeLabel.setText("Snyk Code enabled:");
        codeLabel.setToolTipText("Enable/Disable Snyk Code Scans via Language Server.");

        codeEnabled = new Button(composite, SWT.CHECK);
        codeEnabled.setSelection(false);
        
        Label iacLabel = new Label(composite, SWT.NONE);
        iacLabel.setText("Snyk Infrastructure-as-Code enabled:");
        iacLabel.setToolTipText("Enable/Disable Snyk IaC Scans via Language Server.");

        iacEnabled = new Button(composite, SWT.CHECK);
        iacEnabled.setSelection(false);
        
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        openSourceEnabled.setLayoutData(gd);
        codeEnabled.setLayoutData(gd);
        iacEnabled.setLayoutData(gd);
        
        // required to avoid an error in the system
        setControl(composite);
        setPageComplete(false);
    }
	
	public void handleEvent(Event e) {
	  getWizard().getContainer().updateButtons();
	}
    
	public boolean canFlipToNextPage() {
	    return true;
	}
	
	public IWizardPage getNextPage() {
	  updateModel();
	    
	  return ((SnykWizard)getWizard()).configureAdvance;
	}
	
	private void updateModel() {
	  SnykWizard wizard = (SnykWizard)getWizard();
	  wizard.model.openSourceEnabled = openSourceEnabled.getSelection();
	  wizard.model.codeEnabled = codeEnabled.getSelection();
	  wizard.model.iacEnabled = iacEnabled.getSelection();
	}
}
