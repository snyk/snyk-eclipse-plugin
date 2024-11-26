package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;
import org.eclipse.ui.part.ViewPart;

import com.google.gson.Gson;

import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.views.snyktoolview.providers.TreeContentProvider;
import io.snyk.eclipse.plugin.views.snyktoolview.providers.TreeLabelProvider;
import io.snyk.languageserver.protocolextension.messageObjects.FolderConfig;

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
	private BrowserHandler browserHandler;
	private Map<TreeItem, Listener> itemListeners = new HashMap<>();
	private final Gson gson = new Gson();

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
		treeViewer.setInput(new RootNode());
		treeViewer.expandAll();

		registerTreeContextMeny(parent);

		// Create Browser
		// SWT.EDGE will be ignored if OS not windows and will be set to SWT.NONE.
		browser = new Browser(sashForm, SWT.EDGE);
		browserHandler = new BrowserHandler(browser);
		browserHandler.initialize();
		// Set sash weights
		sashForm.setWeights(new int[] { 1, 2 });

		// Add selection listener to the tree
		treeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@SuppressWarnings("restriction")
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				Display.getDefault().asyncExec(() -> {
					IStructuredSelection selection = (IStructuredSelection) event.getSelection();
					if (selection.isEmpty())
						return;
					TreeNode node = (TreeNode) selection.getFirstElement();
					browserHandler.updateBrowserContent(node);
					if (node instanceof IssueTreeNode) {
						IssueTreeNode issueTreeNode = (IssueTreeNode) node;
						FileTreeNode fileNode = (FileTreeNode) issueTreeNode.getParent();
						LSPEclipseUtils.open(fileNode.getPath().toUri().toASCIIString(),
								issueTreeNode.getIssue().getLSP4JRange());
					}
				});
			}
		});
	}

	private void registerTreeContextMeny(Composite parent) {
		MenuManager menuMgr = new MenuManager("treemenu");
		Menu menu = menuMgr.createContextMenu(parent);
		getSite().registerContextMenu(menuMgr, null);
		parent.setMenu(menu);
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

		for (TreeNode child : getRoot().getChildren()) {
			if (child instanceof ContentRootNode) {
				ContentRootNode contentRoot = (ContentRootNode) child;
				var givenPath = Paths.get(folderPath);
				if (contentRoot.getPath().toString().isBlank()) {
					throw new RuntimeException("unexpected blank content root node " + child);
				}
				if (givenPath.startsWith(contentRoot.getPath())) {
					return contentRoot.getProductNode(product);
				}
			}
		}
		return null;
	}

	@Override
	public BaseTreeNode getRoot() {
		return ((RootNode) treeViewer.getInput());
	}

	@Override
	public void refreshTree() {
		Display.getDefault().asyncExec(() -> {
			this.treeViewer.refresh(true);
		});
	}

	@Override
	public void resetNode(BaseTreeNode node) {
		if (node != null)
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
		var root = getRoot();
		if (root != null) {
			root.reset();
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

	public void enableDelta() {
		TreeItem[] rootItems = getTreeViewer().getTree().getItems();
		for (TreeItem item : rootItems) {
			ContentRootNode node = (ContentRootNode) item.getData();
			String project = node.getText().toString();
			String projectPath = node.getPath().toString();
			String baseBranch = getBaseBranch(projectPath.toString());

			item.setText(String.format("Click to choose base branch for: %s [ current: %s ]", project, baseBranch));

			Listener selectionListener = new Listener() {
				@Override
				public void handleEvent(Event event) {
					if (event.item == item) {
						showPopup(event.display, item);
					}
				}
			};

			// Store the listener in the map
			itemListeners.put(item, selectionListener);

			// Add the listener to the item's parent
			item.getParent().addListener(SWT.Selection, selectionListener);
		}
	}

	private void showPopup(Display display, TreeItem item) {
		Shell shell = new Shell(display, SWT.APPLICATION_MODAL | SWT.DIALOG_TRIM);
		shell.setText("Choose base branch for net-new issues scanning");
		shell.setLayout(new GridLayout(1, false));

		if (!(item.getData() instanceof ContentRootNode))
			return;

		ContentRootNode node = (ContentRootNode) item.getData();
		Path project = node.getPath();

		Label label = new Label(shell, SWT.NONE);
		label.setText("Base Branch for: " + project);
		label.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		List<String> dropdownItems = getLocalBranches(project.toString());

		Combo dropdown = new Combo(shell, SWT.DROP_DOWN);
		dropdown.setItems(dropdownItems.toArray(new String[0]));
		dropdown.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		Button okButton = new Button(shell, SWT.PUSH);
		okButton.setText("OK");
		okButton.setLayoutData(new GridData(SWT.END, SWT.CENTER, false, false));
		okButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				// Handle OK button press
				shell.close();
			}
		});

		shell.pack();
		shell.open();

		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
	}

	public List<String> getLocalBranches(String folderPath) {

		IEclipsePreferences state = InstanceScope.INSTANCE.getNode("io.snyk.eclipse.plugin");
		// Retrieve the JSON string from the preferences state
		String json = state.get(folderPath, null);

		if (json != null) {
			// Deserialize the JSON string back to a FolderConfig object
			FolderConfig folderConfig = gson.fromJson(json, FolderConfig.class);
			// Return the list of local branches
			return folderConfig.getLocalBranches();
		}

		return List.of(); // Return an empty list if no data is found
	}

	public String getBaseBranch(String folderPath) {

		IEclipsePreferences state = InstanceScope.INSTANCE.getNode("io.snyk.eclipse.plugin");
		// Retrieve the JSON string from the preferences state
		String json = state.get(folderPath, null);

		if (json == null)
			return null;

		// Deserialize the JSON string back to a FolderConfig object
		FolderConfig folderConfig = gson.fromJson(json, FolderConfig.class);
		// Return the list of local branches
		return folderConfig.getBaseBranch();

	}

	public void disableDelta() {
		for (Map.Entry<TreeItem, Listener> entry : itemListeners.entrySet()) {
			TreeItem item = entry.getKey();
			Listener listener = entry.getValue();

			// Revert text to original
			item.setText("Project name");

			// Remove listener from the item's parent
			item.getParent().removeListener(SWT.Selection, listener);
		}

		// Clear the map after removing all listeners
		itemListeners.clear();
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
