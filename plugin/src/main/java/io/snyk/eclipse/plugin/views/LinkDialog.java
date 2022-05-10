package io.snyk.eclipse.plugin.views;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Shell;


public class LinkDialog extends Dialog {

  private final String projectURL;
  private final String message;
  private final String title;

  /**
   * Create the dialog.
   *
   * @param parentShell
   */
  public LinkDialog(Shell parentShell, String title, String message, String projectURL) {
    super(parentShell);
    this.message = message;
    this.projectURL = projectURL;
    this.title = title;
  }

  @Override
  protected void configureShell(Shell newShell) {
    super.configureShell(newShell);
    newShell.setText(title);
  }

  /**
   * Create contents of the dialog.
   *
   * @param parent
   */
  @Override
  protected Control createDialogArea(Composite parent) {
    Composite container = (Composite) super.createDialogArea(parent);

    Link link = new Link(container, SWT.NONE);
    link.setText(message + "\n<a href=\"" + projectURL + "\">" + projectURL + "</a>");
    link.addListener(SWT.Selection, event -> org.eclipse.swt.program.Program.launch(event.text));

    link.setSize(330, 150);

    return container;
  }

  /**
   * Create contents of the button bar.
   *
   * @param parent
   */
  @Override
  protected void createButtonsForButtonBar(Composite parent) {
    Button button = createButton(parent, IDialogConstants.OK_ID, "OK", true);
    button.setEnabled(true);
  }

  /**
   * Return the initial size of the dialog.
   */
  @Override
  protected Point getInitialSize() {
    return new Point(850, 180);
  }

}
