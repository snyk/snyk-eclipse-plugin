package io.snyk.eclipse.plugin.utils;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

public class SnykMessageDialog {

	public static void showOkDialog(Shell parentShell, String message) {
		String title = "Information";

		MessageDialog.openInformation(parentShell, title, message);
	}
}
