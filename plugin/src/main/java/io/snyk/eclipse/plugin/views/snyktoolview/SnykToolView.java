/**
 *
 */
package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Paths;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.resource.ImageDescriptor;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.osgi.framework.Bundle;

import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.views.snyktoolview.providers.TreeContentProvider;
import io.snyk.eclipse.plugin.views.snyktoolview.providers.TreeLabelProvider;
import io.snyk.languageserver.protocolextension.FileTreeNode;

/**
 * TODO This view will replace the old SnykView. Move the snyktoolview classes
 * and packages to io.snyk.eclipse.plugin.views, when the original SnykView is
 * removed.
 */
public class SnykToolView extends ViewPart implements ISnykToolView {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "io.snyk.eclipse.plugin.views.snyktoolview";

	ResourceUtils data = new ResourceUtils();

	private Action openPrefPage;

	private TreeViewer treeViewer;
	private Browser browser;
	private BaseTreeNode rootObject = new RootNode();

	private final static Shell SHELL = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

	@Override
	public void createPartControl(Composite parent) {
		SashForm sashForm = new SashForm(parent, SWT.HORIZONTAL);
		sashForm.setLayout(new FillLayout());

		// Create TreeViewer
		treeViewer = new TreeViewer(sashForm, SWT.BORDER);
		Tree tree = treeViewer.getTree();
		tree.setHeaderVisible(false);
		tree.setLinesVisible(false);
		tree.setLayout(new GridLayout());
		tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		// Set up the tree content (replace with your own content provider)
		treeViewer.setContentProvider(new TreeContentProvider());
		treeViewer.setLabelProvider(new TreeLabelProvider());

		// Create and set the root object
		treeViewer.setInput(rootObject);

		registerTreeContextMeny(parent);

		// Create Browser
		browser = new Browser(sashForm, SWT.NONE);
		initBrowserText();

		// Set sash weights
		sashForm.setWeights(new int[] { 1, 2 });

		// Add selection listener to the tree
		treeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@SuppressWarnings("restriction")
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();
				if (!selection.isEmpty()) {
					TreeNode node = (TreeNode) selection.getFirstElement();
					updateBrowserContent(node);
					if (node instanceof IssueTreeNode) {
						IssueTreeNode issueTreeNode = (IssueTreeNode) node;
						FileTreeNode fileNode = (FileTreeNode) issueTreeNode.getParent();
						LSPEclipseUtils.open(fileNode.getPath().toUri().toASCIIString(),
								issueTreeNode.getIssue().getLSP4JRange());
					}
				}
			}
		});
	}

	private void registerTreeContextMeny(Composite parent) {
		MenuManager menuMgr = new MenuManager("treemenu");
		Menu menu = menuMgr.createContextMenu(parent);
		getSite().registerContextMenu(menuMgr, null);
		parent.setMenu(menu);
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
		if (node instanceof BaseTreeNode) {
			return ((BaseTreeNode) node).getDetails();
		}
		return "";
	}

	private String generateHtmlContent(String text) {
		return "<html><body<p>" + text + "</p></body></html>";
	}

	private void initBrowserText() {
		String snykWarningText = Platform.getResourceString(Platform.getBundle("io.snyk.eclipse.plugin"),
				"%snyk.trust.dialog.warning.text");

		Bundle bundle = Platform.getBundle("io.snyk.eclipse.plugin");
		String base64Image = ResourceUtils.getBase64Image(bundle, "logo_snyk.png");

		browser.setText("<!DOCTYPE html> <html lang=\"en\"> <head> <meta charset=\"UTF-8\"> "
				+ "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"> "
				+ "<title>Snyk for Eclipse</title> <style> .container { display: flex; align-items: center; } .logo { margin-right: 20px; } "
				+ "</style> </head> <body> <div class=\"container\"> " + "<img src='data:image/png;base64,"
				+ base64Image + "' alt='Snyk Logo'>" + "<div> <p><strong>Welcome to Snyk for Eclipse</strong></p>"
				+ "    <p>\n" + snykWarningText + "</body>\n" + "</html>");
	}

	@Override
	public void setFocus() {
		treeViewer.getControl().setFocus();
	}

	@Override
	public void setNodeText(BaseTreeNode node, String text) {
		node.setText(text);
		Display.getDefault().asyncExec(() -> {
			this.treeViewer.refresh(node, true);
		});
	}

	@Override
	public void addIssueNode(FileTreeNode parent, IssueTreeNode toBeAdded) {
		toBeAdded.setParent(parent);
		parent.addChild(toBeAdded);
		Display.getDefault().asyncExec(() -> {
			this.treeViewer.refresh(parent, true);
		});
	}

	@Override
	public void addFileNode(ProductTreeNode parent, FileTreeNode toBeAdded) {
		toBeAdded.setParent(parent);
		parent.addChild(toBeAdded);
		Display.getDefault().asyncExec(() -> {
			this.treeViewer.refresh(parent, true);
		});
	}

	@Override
	public void addInfoNode(ProductTreeNode parent, InfoTreeNode toBeAdded) {
		toBeAdded.setParent(parent);
		parent.addChild(toBeAdded);
		Display.getDefault().asyncExec(() -> {
			this.treeViewer.refresh(parent, true);
		});
	}

	@Override
	public ProductTreeNode getProductNode(String product, String folderPath) {
		if (product == null || folderPath == null) {
			return null;
		}

		for (TreeNode child : rootObject.getChildren()) {
			if (child instanceof ContentRootNode) {
				ContentRootNode contentRoot = (ContentRootNode) child;
				var givenPath = Paths.get(folderPath);
				if (givenPath.startsWith(contentRoot.getPath())) {
					return contentRoot.getProductNode(product);
				}
			}
		}
		return null;
	}

	@Override
	public BaseTreeNode getRoot() {
		return this.rootObject;
	}

	@Override
	public void refreshTree() {
		Display.getDefault().asyncExec(() -> {
			this.treeViewer.refresh(true);
		});
	}

	@Override
	public void resetNode(BaseTreeNode node) {
		node.reset();
		Display.getDefault().asyncExec(() -> {
			this.treeViewer.refresh(node, true);
		});
	}

	@Override
	public void removeInfoNodes(ProductTreeNode node) {
		node.removeInfoNodes();
		Display.getDefault().asyncExec(() -> {
			this.treeViewer.refresh(node, true);
		});
	}

	public void clearTree() {
		clearRoot();
		Display.getDefault().asyncExec(() -> {
			if (this.treeViewer != null && !this.treeViewer.getTree().isDisposed()) {
				this.treeViewer.refresh();
				this.treeViewer.expandAll(); // Optional: if you want to expand all nodes after clearing
			}
		});
	}

	private void clearRoot() {
		if (this.rootObject != null) {
			this.rootObject.removeChildren();
			this.rootObject.reset();
		}
	}

	@Override
	public TreeViewer getTreeViewer() {
		return this.treeViewer;
	}
}