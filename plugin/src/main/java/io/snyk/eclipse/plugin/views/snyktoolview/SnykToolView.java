/**
 *
 */
package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Paths;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;
import org.eclipse.ui.part.ViewPart;
import org.osgi.framework.Bundle;

import io.snyk.eclipse.plugin.html.HtmlProviderFactory;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.views.snyktoolview.providers.TreeContentProvider;
import io.snyk.eclipse.plugin.views.snyktoolview.providers.TreeLabelProvider;

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
		treeViewer.expandAll();

		registerTreeContextMeny(parent);

		// Create Browser
		// SWT.EDGE will be ignored if OS not windows and will be set to SWT.NONE.
		browser = new Browser(sashForm, SWT.EDGE);
		// Register the Java function as an anonymous class

		new BrowserFunction(browser, "openInEditor") {
		    @SuppressWarnings("restriction")
			@Override
		    public Object function(Object[] arguments) {
		        if (arguments.length != 5) {
		        	return null;
		        }
	            String filePath = (String) arguments[0];
	            var fileUri = Paths.get(filePath).toUri().toASCIIString();
	            int startLine = Integer.parseInt(arguments[1].toString());
	            int endLine = Integer.parseInt(arguments[2].toString());
	            int startCharacter = Integer.parseInt(arguments[3].toString());
	            int endCharacter = Integer.parseInt(arguments[4].toString());

	            Display.getDefault().asyncExec(() -> {
	                try {
	                    Position startPosition = new Position(startLine, startCharacter);
	                    Position endPosition = new Position(endLine, endCharacter);
	                    Range range = new Range(startPosition, endPosition);

	                    var location = new Location(fileUri, range);
	                    LSPEclipseUtils.openInEditor(location);

	                } catch (Exception e) {
	                    e.printStackTrace();
	                }
	            });
		        return null;
		    }
		};

        browser.addLocationListener(new LocationListener() {
            @Override
            public void changing(LocationEvent event) {
                String url = event.location;
                if(url.startsWith("http")) {
                	event.doit = false;
                    Program.launch(url);
                }
            }

            @Override
            public void changed(LocationEvent event) {
            }
        });
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
		if (node instanceof IssueTreeNode) {
			var product = ((ProductTreeNode) node.getParent().getParent()).getProduct();
			var htmlProvider = HtmlProviderFactory.GetHtmlProvider(product);
			htmlContent = htmlProvider.replaceCssVariables(htmlContent);
			browser.setText(htmlContent);
			browser.execute(htmlProvider.getInitScript());
		}
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
			this.treeViewer.expandToLevel(4); // Expand to level 4 to show ProjectNode, ProductNodes, FileNode, and
												// IssueNodes.
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

	@Override
	public void toggleIgnoresButtons() {

		Display.getDefault().asyncExec(() -> {
			IMenuManager menuManager = this.getViewSite().getActionBars().getMenuManager();
			IMenuManager submenu = menuManager
					.findMenuUsingPath("io.snyk.eclipse.plugin.views.snyktoolview.filterIgnoreMenu");

			if (submenu == null) {
				return;
			}

			boolean enableConsistentIgnores = Preferences.getInstance()
					.getBooleanPref(Preferences.IS_GLOBAL_IGNORES_FEATURE_ENABLED, false);

			if (enableConsistentIgnores) {
				// Show the commands
				addCommandIfNotPresent(submenu, "io.snyk.eclipse.plugin.commands.snykShowOpenIgnored");
				addCommandIfNotPresent(submenu, "io.snyk.eclipse.plugin.commands.snykShowIgnored");
			} else {
				// Hide the commands
				submenu.remove("io.snyk.eclipse.plugin.commands.snykShowOpenIgnored");
				submenu.remove("io.snyk.eclipse.plugin.commands.snykShowIgnored");
			}

			menuManager.update(true);

		});

	}

	// Helper method to add a command if it's not already present
	private void addCommandIfNotPresent(IMenuManager menu, String commandId) {
		if (menu.find(commandId) == null) {
			CommandContributionItemParameter param = new CommandContributionItemParameter(PlatformUI.getWorkbench(),
					null, commandId, CommandContributionItem.STYLE_PUSH);
			CommandContributionItem item = new CommandContributionItem(param);
			menu.add(item);
		}
	}

}
