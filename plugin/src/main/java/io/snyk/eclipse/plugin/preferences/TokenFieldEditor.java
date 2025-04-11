package io.snyk.eclipse.plugin.preferences;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;

public class TokenFieldEditor extends StringFieldEditor {
	private Preferences preferences;

	protected TokenFieldEditor(Preferences preferences, String name, String labelText, Composite parent) {
		super(name, labelText, 80, parent);
		this.preferences = preferences;
		super.setPreferenceStore(preferences.getSecureStore()); // NOPMD
		getTextControl().setEchoChar('*'); // NOPMD
	}

	public void emptyTextfield() {
		setStringValue("");
	}

	@Override
	public void setPreferenceStore(IPreferenceStore store) {
		// we don't let the page override the preference store to a non-secure store
		super.setPreferenceStore(preferences.getSecureStore());
	}
}
