/**
 *
 */
package io.snyk.eclipse.plugin.views.snyktoolview;

import javax.inject.Inject;

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
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.part.ViewPart;

/**
 * The view will replace the old SnykView. TODO move the snyktoolview classes
 * and packages to io.snyk.eclipse.plugin.views, when the original SnykView is
 * removed.
 */
public class SnykToolView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "io.snyk.eclipse.plugin.views.snyktoolview.SnykToolView";

	@Inject
	IWorkbench workbench;

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
		return "<html><body><h1>" + "</h1><p>Content for " + "</p></body></html>";
	}

	private void initBrowserText() {

		browser.setText("<!DOCTYPE html>\n" + "<html lang=\"en\">\n" + "<head>\n" + "    <meta charset=\"UTF-8\">\n"
				+ "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
				+ "    <title>Snyk for JetBrains</title>\n" + "</head>\n" + "<body>\n"
				+ "    <p><strong>Welcome to Snyk for JetBrains</strong></p>\n" + "\n" + "    <ol>\n"
				+ "        <li align=\"left\">Authenticate to Snyk.io</li>\n"
				+ "        <li align=\"left\">Analyze code for issues and vulnerabilities</li>\n"
				+ "        <li align=\"left\">Improve your code and upgrade dependencies</li>\n" + "    </ol>\n" + "\n"
				+ "    <p>\n"
				+ "        When scanning project files for vulnerabilities, Snyk may automatically execute code such as invoking the package manager to get dependency information.<br><br>You should only scan projects you trust.<br><br>\n"
				+ "</body>\n" + "</html>");
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