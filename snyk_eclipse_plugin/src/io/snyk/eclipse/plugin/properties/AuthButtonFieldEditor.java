package io.snyk.eclipse.plugin.properties;

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
        getTextControl().setEditable(false);
    }

	@Override
	protected String changePressed() {
		try {
			String apiKey = Authenticator.INSTANCE.callLogin();
			Authenticator.INSTANCE.auth();
			return apiKey;
		} catch (AuthException e) {
			e.printStackTrace();
			MessageDialog.openError(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Authentication", e.getMessage());
			return null;
		}
	}

	

}
