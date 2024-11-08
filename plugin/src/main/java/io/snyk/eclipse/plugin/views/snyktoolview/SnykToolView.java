/**
 *
 */
package io.snyk.eclipse.plugin.views.snyktoolview;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.util.Base64;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.part.ViewPart;
import org.osgi.framework.Bundle;

import io.snyk.eclipse.plugin.utils.ResourceUtils;

/**
 * TODO This view will replace the old SnykView. Move the snyktoolview classes
 * and packages to io.snyk.eclipse.plugin.views, when the original SnykView is
 * removed.
 */
public class SnykToolView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView";

	ResourceUtils data = new ResourceUtils();

	private Action openPrefPage;

	private TreeViewer treeViewer;
	private Browser browser;

	private final static Shell SHELL = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

	@Override
	public void createPartControl(Composite parent) {
		SashForm sashForm = new SashForm(parent, SWT.HORIZONTAL);
		sashForm.setLayout(new FillLayout());

		// Create TreeViewer
		treeViewer = new TreeViewer(sashForm, SWT.BORDER);
		Tree tree = treeViewer.getTree();
		tree.setHeaderVisible(true);
		tree.setLinesVisible(true);

		// Set up the tree content (replace with your own content provider)
//		treeViewer.setContentProvider(new TreeContentProvider());
//		treeViewer.setLabelProvider(new TreeLabelProvider());
//		treeViewer.setInput(createTreeInput());

		// Create Browser
		browser = new Browser(sashForm, SWT.NONE);
		initBrowserText();

		// Set sash weights
		sashForm.setWeights(new int[] { 1, 2 });

		// Add selection listener to the tree
		treeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();
				if (!selection.isEmpty()) {
					TreeNode node = (TreeNode) selection.getFirstElement();
					updateBrowserContent(node);
				}
			}
		});

		makeActions();
	}

	private void updateBrowserContent(TreeNode node) {
		// Generate HTML content based on the selected node
		String htmlContent = generateHtmlContent(node);
		browser.setText(htmlContent);
	}

	private void updateBrowserContent(String text) {
		String htmlContent = generateHtmlContent(text);
		browser.setText(htmlContent);
	}

	private String generateHtmlContent(TreeNode node) {
		// return "<html><body><h1>" + "</h1><p>Content for " + "</p></body></html>";
		return "<html><body><h1>" + node.getValue() + "</h1><p>Content for " + node.getValue() + "</p></body></html>";
	}

	private String generateHtmlContent(String text) {
		return "<html><body<p>" + text + "</p></body></html>";
	}

	private void initBrowserText() {
		String snykWarningText = Platform.getResourceString(Platform.getBundle("io.snyk.eclipse.plugin"),
				"%snyk.trust.dialog.warning.text");

		Bundle bundle = Platform.getBundle("io.snyk.eclipse.plugin");
		String base64Image = ResourceUtils.getBase64Image( bundle, "logo_snyk.png" );

		browser.setText("<!DOCTYPE html> <html lang=\"en\"> <head> <meta charset=\"UTF-8\"> "
				+ "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"> "
				+ "<title>Snyk for JetBrains</title> <style> .container { display: flex; align-items: center; } .logo { margin-right: 20px; } "
				+ "</style> </head> <body> <div class=\"container\"> " + "<img src='data:image/png;base64,"
				+ base64Image + "' alt='Snyk Logo'>" + "<div> <p><strong>Welcome to Snyk for JetBrains</strong></p>"
				+ "    <p>\n" + snykWarningText + "</body>\n" + "</html>");
	}

	private Object createTreeInput() {
		// Create and return your tree structure here
		TreeNode root = new TreeNode("Root");

		TreeNode[] nodeList = new TreeNode[10];

		nodeList[0] = new TreeNode("Node 1");
		nodeList[1] = new TreeNode("Node 2");
		nodeList[2] = new TreeNode("Node 3");
		nodeList[3] = new TreeNode("Node 4");
		nodeList[4] = new TreeNode("Node 5");
		nodeList[5] = new TreeNode("Node 6");
		nodeList[6] = new TreeNode("Node 7");
		nodeList[7] = new TreeNode("Node 8");
		nodeList[8] = new TreeNode("Node 9");
		nodeList[9] = new TreeNode("Node 10");

		return root;
	}

	private void makeActions() {
		openPrefPage = new Action() {

			@Override
			public void run() {
				PreferenceDialog pref = PreferencesUtil.createPreferenceDialogOn(getShell(),
						"io.snyk.eclipse.plugin.properties.preferencespage", null, null);
				if (pref != null)
					pref.open();
			}
		};
		openPrefPage.setText("Preferences");
	}

	
	private static Shell getShell() {
		var activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null)
			return SHELL;
		return activeWorkbenchWindow.getShell();
	}

	@Override
	public void setFocus() {
		treeViewer.getControl().setFocus();
	}
}