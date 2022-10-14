package io.snyk.eclipse.plugin.wizards;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class SnykWizardWelcome extends WizardPage {
	private Text welcomeMsg;
	private Composite container;

	public SnykWizardWelcome() {
		super("Snyk Wizard");
		setTitle("Configure Snyk");
		setDescription("Welcome to the Snyk configuration wizard");
	}
	
	@Override
	public void createControl(Composite parent) {
		container = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		container.setLayout(layout);
		layout.numColumns = 2;
		Label welcomeLabel = new Label(container, SWT.NONE);
		welcomeLabel.setText("Welcome");
		
		welcomeMsg = new Text(container, SWT.BORDER | SWT.SINGLE);
		welcomeMsg.setText("Welcome, the Snyk configuration wizard should help you get setup and start using Snyk.");
		welcomeMsg.addKeyListener(new KeyListener() {
			@Override
			public void keyPressed(KeyEvent e) {}

			@Override
			public void keyReleased(KeyEvent e) {
				if (!welcomeMsg.getText().isEmpty()) {
					setPageComplete(true);

				}
			}	
		});
      GridData gd = new GridData(GridData.FILL_HORIZONTAL);
      welcomeMsg.setLayoutData(gd);
      // required to avoid an error in the system
      setControl(container);
      setPageComplete(false);
  }

  public String getWelcomeMsg() {
      return welcomeMsg.getText();
  }
}
