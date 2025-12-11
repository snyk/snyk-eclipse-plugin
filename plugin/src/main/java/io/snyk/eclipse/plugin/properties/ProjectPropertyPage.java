package io.snyk.eclipse.plugin.properties;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.dialogs.PreferencesUtil;

/**
 * Project property page for Snyk that directs users to the main Snyk preferences.
 */
public class ProjectPropertyPage extends PreferencePage implements IWorkbenchPropertyPage {

	private static final String SNYK_PREFERENCES_PAGE_ID = "io.snyk.eclipse.plugin.properties.preferencespage";
	private IAdaptable element;

	public ProjectPropertyPage() {
		super();
		noDefaultAndApplyButton();
	}

	@Override
	protected Control createContents(Composite parent) {
		Composite container = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1, false);
		layout.marginWidth = 10;
		layout.marginHeight = 10;
		layout.verticalSpacing = 15;
		container.setLayout(layout);

		Label infoLabel = new Label(container, SWT.WRAP);
		infoLabel.setText("Snyk project settings are configured in the Snyk Plugin Preferences.");
		GridData labelData = new GridData(SWT.FILL, SWT.TOP, true, false);
		labelData.widthHint = 400;
		infoLabel.setLayoutData(labelData);

		Link preferencesLink = new Link(container, SWT.NONE);
		preferencesLink.setText("<a>Open Snyk Preferences</a>");
		preferencesLink.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				PreferencesUtil.createPreferenceDialogOn(
						parent.getShell(),
						SNYK_PREFERENCES_PAGE_ID,
						new String[] { SNYK_PREFERENCES_PAGE_ID },
						null
				).open();
			}
		});

		return container;
	}

	@Override
	public IAdaptable getElement() {
		return this.element;
	}

	@Override
	public void setElement(IAdaptable element) {
		this.element = element;
	}
}
