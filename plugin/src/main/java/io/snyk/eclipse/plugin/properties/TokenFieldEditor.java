package io.snyk.eclipse.plugin.properties;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;


public class TokenFieldEditor extends StringFieldEditor {

  private final io.snyk.eclipse.plugin.properties.preferences.Preferences store;

  protected TokenFieldEditor(io.snyk.eclipse.plugin.properties.preferences.Preferences store, String name, String labelText,
                             Composite parent) {
    super(name, labelText, 80, parent);
    this.store = store;
    getTextControl().setEchoChar('*');
  }

  public void emptyTextfield() {
    setStringValue("");
    store.store(Preferences.AUTH_TOKEN_KEY, "");
  }


}
