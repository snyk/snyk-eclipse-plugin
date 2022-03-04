package io.snyk.eclipse.plugin.properties;

import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;


public class TokenFieldEditor extends StringFieldEditor {

    private final Preferences store;

    protected TokenFieldEditor(Preferences store, String name, String labelText,
                               Composite parent) {
        super(name, labelText, parent);
        this.store = store;
        getTextControl().setEchoChar('*');
    }

    public void emptyTextfield() {
        setStringValue("");
        store.store(Preferences.AUTH_TOKEN_KEY, "");
    }


}
