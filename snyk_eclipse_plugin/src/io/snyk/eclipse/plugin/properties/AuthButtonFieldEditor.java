package io.snyk.eclipse.plugin.properties;

import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.StringButtonFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;

import io.snyk.eclipse.plugin.exception.AuthException;
import io.snyk.eclipse.plugin.runner.Authenticator;


public class AuthButtonFieldEditor extends StringButtonFieldEditor{
	
    protected AuthButtonFieldEditor(String name, String labelText,
            Composite parent) {
        super(name, labelText, parent);
        setChangeButtonText("Authenticate");
        // workaround: enable token textfield until issue with SSO is solved
        // getTextControl().setEditable(false);
    }

	@Override
	protected String changePressed() {
		MessageDialog.openInformation(null, "Authentication", "Please enter your Snyk API Token into the text field");
		return null;
//		try {
//			PreferencesPage page = (PreferencesPage) getPage();
//			page.persist();
//
//			return Authenticator.INSTANCE.callLogin();
//		} catch (AuthException e) {
//			e.printStackTrace();
//			MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Authentication", e.getMessage());
//			return null;
//		}
	}
	
	public void emptyTextfield() {
		setStringValue("");
		Preferences.store(Preferences.AUTH_TOKEN_KEY, "");
	}

}
