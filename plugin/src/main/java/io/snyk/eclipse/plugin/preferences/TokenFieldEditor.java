package io.snyk.eclipse.plugin.preferences;

import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;


public class TokenFieldEditor extends StringFieldEditor {
  protected TokenFieldEditor(Preferences preferences, String name, String labelText,
                             Composite parent) {
    super(name, labelText, 80, parent);
    setPreferenceStore(preferences.getSecureStore()); //NOPMD
    getTextControl().setEchoChar('*'); //NOPMD
  }

  public void emptyTextfield() {
    setStringValue("");
  }


}
