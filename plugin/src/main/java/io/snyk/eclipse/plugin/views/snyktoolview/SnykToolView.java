package io.snyk.eclipse.plugin.views.snyktoolview;

import static io.snyk.eclipse.plugin.domain.ProductConstants.SCAN_STATE_IN_PROGRESS;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;
import org.eclipse.ui.part.ViewPart;

import io.snyk.eclipse.plugin.preferences.Preferences;
import io.snyk.eclipse.plugin.properties.FolderConfigs;
import io.snyk.eclipse.plugin.utils.ResourceUtils;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.snyktoolview.providers.TreeContentProvider;
import io.snyk.eclipse.plugin.views.snyktoolview.providers.TreeLabelProvider;
import io.snyk.languageserver.CommandHandler;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;
import io.snyk.languageserver.protocolextension.messageObjects.scanResults.Issue;

public class SnykToolView extends ViewPart implements ISnykToolView {
	public SnykToolView() {
	}

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "io.snyk.eclipse.plugin.views.snyktoolview";

	private TreeViewer treeViewer;
	private Browser browser;
	private BrowserHandler browserHandler;
	private Browser summaryBrowser;
	private SummaryBrowserHandler summaryBrowserHandler;
	private TreeNode selectedNode;

	@Override
	public void createPartControl(Composite parent) {
		SashForm horizontalSashForm = new SashForm(parent, SWT.HORIZONTAL);
		horizontalSashForm.setLayout(new FillLayout());

		// Create vertical SashForm for new Browser and TreeViewer
		SashForm verticalSashForm = new SashForm(horizontalSashForm, SWT.VERTICAL);
		verticalSashForm.setLayout(new FillLayout());

		// Create new Browser (top quarter)
		summaryBrowser = new Browser(verticalSashForm, SWT.EDGE);
		summaryBrowserHandler = new SummaryBrowserHandler(summaryBrowser);
		summaryBrowserHandler.initialize();

		// Create TreeViewer
		treeViewer = new TreeViewer(verticalSashForm, SWT.BORDER);
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

		registerTreeContextMenu(treeViewer.getControl());

		verticalSashForm.setWeights(1, 3);

		// Create Browser
		// SWT.EDGE will be ignored if OS not windows and will be set to SWT.NONE.
		browser = new Browser(horizontalSashForm, SWT.EDGE);
		browserHandler = new BrowserHandler(browser);
		browserHandler.initialize();
		// Set sash weights
		horizontalSashForm.setWeights(1, 2);

		// Add selection listener to the tree
		treeViewer.addSelectionChangedListener(new ISelectionChangedListener() {

			@SuppressWarnings("restriction")
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				Display.getDefault().asyncExec(() -> {
					IStructuredSelection selection = (IStructuredSelection) event.getSelection();
					if (selection.isEmpty())
						return;
					selectedNode = (TreeNode) selection.getFirstElement();
					if (selectedNode instanceof IssueTreeNode) {
						IssueTreeNode issueTreeNode = (IssueTreeNode) selectedNode;
						FileTreeNode fileNode = (FileTreeNode) issueTreeNode.getParent();
						LSPEclipseUtils.open(fileNode.getPath().toUri().toASCIIString(),
								issueTreeNode.getIssue().getLSP4JRange());
					}
					boolean deltaEnabled = Preferences.isDeltaEnabled();
					if (selectedNode instanceof ContentRootNode && deltaEnabled) {
						ContentRootNode contentNode = (ContentRootNode) selectedNode;
						new ReferenceChooserDialog(Display.getDefault().getActiveShell(), contentNode.getPath()).open();
					} else {
						// update the browser content for everything but content roots
						browserHandler.updateBrowserContent(selectedNode);
					}

				});
			}
		});

		if (Preferences.isDeltaEnabled())
			this.enableDelta();

		// initialize the filters
		TreeFilterManager.getInstance();
	}

	private void registerTreeContextMenu(Control control) {
		MenuManager menuMgr = new MenuManager("treemenu");
		Menu menu = menuMgr.createContextMenu(control);
		getSite().registerContextMenu(menuMgr, null);
		control.setMenu(menu);

		Action ignoreAction = getIgnoreAction();
		Action monitorAction = getMonitorAction();
		Action openProjectPreferencesAction = getOpenProjectPreferencesAction();

		menuMgr.add(ignoreAction);
		menuMgr.add(monitorAction);
		menuMgr.add(openProjectPreferencesAction);
	}

	private Action getIgnoreAction() {
		return new Action("Ignore issue in .snyk") {
			@Override
			public void run() {
				CompletableFuture.runAsync(() -> {
					IssueTreeNode itn = (IssueTreeNode) selectedNode;
					Issue issue = itn.getIssue();
					new Job("Ignoring issue " + issue.getDisplayTitle()) {

						@Override
						protected IStatus run(IProgressMonitor monitor) {
							try {
								var result = CommandHandler.getInstance().ignoreIssue(issue).get(10, TimeUnit.SECONDS);
								outputCommandResult(result);
								itn.setText("[ IGNORED ] " + itn.getText());
								Display.getDefault().asyncExec(() -> {
									treeViewer.refresh(itn, true);
								});
							} catch (InterruptedException e) {
								Thread.currentThread().interrupt();
								SnykLogger.logError(e);
								return Status.CANCEL_STATUS;
							} catch (ExecutionException | TimeoutException e) {
								SnykLogger.logError(e);
								return Status.CANCEL_STATUS;
							}
							return Status.OK_STATUS;
						}
					}.schedule();
				});
			}

			@Override
			public boolean isEnabled() {
				return selectedNode instanceof IssueTreeNode && CommandHandler.getInstance().canBeIgnored(getProduct());
			}

			protected String getProduct() {
				var pn = (ProductTreeNode) selectedNode.getParent().getParent();
				String product = pn.getProduct();
				return product;
			}
		};
	}

	private Action getMonitorAction() {
		return new Action("Monitor project") {
			@Override
			public void run() {
				CompletableFuture.runAsync(() -> {
					IssueTreeNode itn = (IssueTreeNode) selectedNode;
					Issue issue = itn.getIssue();
					IProject project = ResourceUtils.getProjectByPath(Paths.get(issue.filePath()));
					new Job("Monitoring project " + project.getName()) {

						@Override
						protected IStatus run(IProgressMonitor monitor) {
							try {
								Path workingDir = ResourceUtils.getFullPath(project);
								var result = CommandHandler.getInstance().monitorProject(workingDir).get();
								outputCommandResult(result);
							} catch (InterruptedException e) {
								Thread.currentThread().interrupt();
								SnykLogger.logError(e);
								return Status.CANCEL_STATUS;
							} catch (ExecutionException e) {
								SnykLogger.logError(e);
								return Status.CANCEL_STATUS;
							}
							return Status.OK_STATUS;
						}
					}.schedule();
				});
			}

			@Override
			public boolean isEnabled() {
				return true;
			}
		};
	}

	private Action getOpenProjectPreferencesAction() {
		return new Action("Open Project Preferences") {
			@Override
			public void run() {
				if (selectedNode instanceof ContentRootNode) {
					ContentRootNode contentNode = (ContentRootNode) selectedNode;
					Path projectPath = contentNode.getPath();
					if (projectPath != null) {
						IProject project = ResourceUtils.getProjectByPath(projectPath);
						if (project != null) {
							// Open the project properties dialog and navigate to Snyk preferences
							PreferencesUtil.createPropertyDialogOn(
								getSite().getShell(),
								project,
								"io.snyk.eclipse.plugin.properties.projectpropertypage",
								null,
								null
							).open();
						}
					}
				}
			}

			@Override
			public boolean isEnabled() {
				return selectedNode instanceof ContentRootNode &&
					   ((ContentRootNode) selectedNode).getPath() != null;
			}
		};
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
				if (contentRoot.getPath() != null && givenPath.startsWith(contentRoot.getPath())) {
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
	public void refreshBrowser(String status) {
		Display.getDefault().asyncExec(() -> {
			if (null != status && status.equals(SCAN_STATE_IN_PROGRESS)) {
				this.browserHandler.setScanningBrowserText();
			} else {
				this.browserHandler.setDefaultBrowserText();
			}
		});
	};

	@Override
	public void updateSummary(String summary) {
		Display.getDefault().asyncExec(() -> {
			this.summaryBrowserHandler.setBrowserText(summary);
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

	@Override
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

			// Always show the commands, regardless of CCI status
			addCommandIfNotPresent(submenu, "io.snyk.eclipse.plugin.commands.snykShowOpenIgnored");
			addCommandIfNotPresent(submenu, "io.snyk.eclipse.plugin.commands.snykShowIgnored");

			menuManager.update(true);

		});
	}

	@Override
	public void refreshDeltaReference() {
		BaseTreeNode[] children = (BaseTreeNode[]) getRoot().getChildren();

		for (BaseTreeNode node : children) {
			if (node instanceof ContentRootNode) {
				ContentRootNode contentNode = (ContentRootNode) node;
				if (!contentNode.getLabel().isBlank()) {
					setReferenceText(contentNode);
				}
			}
		}
		refreshTree();
	}

	@Override
	public void enableDelta() {
		if (this.treeViewer != null && !this.treeViewer.getTree().isDisposed()) {
			BaseTreeNode[] children = (BaseTreeNode[]) getRoot().getChildren();

			for (BaseTreeNode node : children) {
				if (node instanceof ContentRootNode) {
					ContentRootNode contentNode = (ContentRootNode) node;
					setReferenceText(contentNode);
				}
			}
		}
	}

	private void setReferenceText(ContentRootNode contentNode) {
		var folderConfig = FolderConfigs.getInstance().getFolderConfig(contentNode.getPath());
		String referenceFolder = folderConfig.getReferenceFolderPath();
		String baseBranch = folderConfig.getBaseBranch();
		var reference = referenceFolder;
		if (reference.isBlank()) {
			reference = baseBranch;
		}
		contentNode.setLabel(String.format(" - Click here to choose reference [ current: %s ]", reference));
	}

	@Override
	public void disableDelta() {
		if (this.treeViewer != null && !this.treeViewer.getTree().isDisposed()) {
			BaseTreeNode[] children = (BaseTreeNode[]) getRoot().getChildren();

			for (BaseTreeNode node : children) {
				if (node instanceof ContentRootNode) {
					ContentRootNode contentNode = (ContentRootNode) node;
					contentNode.setLabel("");
				}
			}
		}
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

	@SuppressWarnings("restriction")
	protected void outputCommandResult(Object result) {
		if (result != null && result instanceof Map) {
			@SuppressWarnings("unchecked")
			Map<String, Object> resultMap = (Map<String, Object>) result;
			String stdOut = resultMap.get("stdOut").toString();
			boolean exitCode = (Double) resultMap.get("exitCode") == 0;
			if (exitCode) {
				MessageParams messageParams = new MessageParams(MessageType.Info, stdOut);
				SnykExtendedLanguageClient.getInstance().showMessage(messageParams);
			} else {
				SnykLogger.logError(new RuntimeException(stdOut));
			}
		}
	}

	@Override
	public void selectTreeNode(Issue issue, String product) {
		ProductTreeNode productNode = getProductNode(product, issue.filePath());
		selectTreenodeForIssue((TreeNode) productNode, issue);
	}

	private void selectTreenodeForIssue(TreeNode currentParent, Issue issue) {
		for (Object child : currentParent.getChildren()) {
			TreeNode childNode = (TreeNode) child;

			if (childNode instanceof IssueTreeNode && ((IssueTreeNode) childNode).getIssue().id().equals(issue.id())) {
				updateSelection((IssueTreeNode) childNode);
				return; // Exit the function as we've found a match
			}

			if (childNode.getChildren() != null && childNode.getChildren().length != 0) {
				selectTreenodeForIssue(childNode, issue);
			}
		}
	}

	private void updateSelection(IssueTreeNode issueTreeNode) {
		Display.getDefault().asyncExec(() -> {
			IStructuredSelection selection = new StructuredSelection(issueTreeNode);
			treeViewer.setSelection(selection, true);
		});
	}
}
