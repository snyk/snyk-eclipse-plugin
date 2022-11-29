/*******************************************************************************
 * Copyright (c) 2005-2008 VecTrace (Zingo Andersen) and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * bastian  implementation
 *******************************************************************************/


package io.snyk.eclipse.plugin.wizards;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

/**
 * @author bastian
 * https://foss.heptapod.net/mercurial/mercurialeclipse/-/blob/branch/default/plugin/src/com/vectrace/MercurialEclipse/ui/SWTWidgetHelper.java
 */
public final class SWTWidgetHelper {
    public static final int LABEL_WIDTH_HINT = 400;
    public static final int LABEL_INDENT_WIDTH = 32;
    public static final int LIST_HEIGHT_HINT = 100;
    public static final int SPACER_HEIGHT = 8;

    private SWTWidgetHelper() {
        // hide constructor of utility class.
    }

    /**
     * Creates a group that <b>has <u>and</u> spans</b> the given number of columns in its parent and which has the
     * given style.
     *
     * @param parent
     *            the parent control.
     * @param text
     *            the title of the group.
     * @param span
     *            the number of columns (in the parent's layout) that this group will span, which is also the number of
     *            columns that this group has.
     * @param style
     *            the chosen style of the grid layout for this group.
     * @return a new group
     */
    public static Group createGroup(Composite parent, String text, int span, int style) {
        Group group = new Group(parent, SWT.NULL);
        group.setText(text);
        GridData data = new GridData(style);
        data.horizontalSpan = span;
        // data.widthHint = GROUP_WIDTH;

        group.setLayoutData(data);
        GridLayout layout = new GridLayout();
        layout.numColumns = span;
        group.setLayout(layout);
        return group;
    }

    /**
     * Creates a group that spans two columns.
     *
     * @param parent
     *            the parent control
     * @param text
     *            the title of the group
     * @param style
     *            the chosen style for this group
     * @return a new group
     */
    public static Group createGroup(Composite parent, String text, int style) {
        return createGroup(parent, text, 2, style);
    }

    /**
     * Creates a group that has two columns and which style is horizontal fill.
     *
     * @param parent
     *            the parent control
     * @param text
     *            the title of the group
     * @return a new group
     */
    public static Group createGroup(Composite parent, String text) {
        return createGroup(parent, text, GridData.FILL_HORIZONTAL);
    }
}
