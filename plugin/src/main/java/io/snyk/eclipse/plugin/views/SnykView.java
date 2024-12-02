package io.snyk.eclipse.plugin.views;

import static io.snyk.eclipse.plugin.utils.FileSystemUtil.getCliFile;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import javax.inject.Inject;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.part.ViewPart;

import io.snyk.eclipse.plugin.Activator;
import io.snyk.eclipse.plugin.domain.MonitorResult;
import io.snyk.eclipse.plugin.properties.preferences.Preferences;
import io.snyk.eclipse.plugin.utils.SnykLogger;
import io.snyk.eclipse.plugin.views.provider.ColumnProvider;
import io.snyk.eclipse.plugin.views.provider.ColumnTextProvider;
import io.snyk.eclipse.plugin.views.provider.LinkLabelProvider;
import io.snyk.eclipse.plugin.views.provider.TreeContentProvider;
import io.snyk.languageserver.protocolextension.SnykExtendedLanguageClient;

public class SnykView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "io.snyk.eclipse.plugin.views.SnykView";

	public static final Image CRITICAL_SEVERITY = Activator.getImageDescriptor("/icons/severity-critical.png")
			.createImage();

	public static final Image HIGH_SEVERITY = Activator.getImageDescriptor("/icons/severity-high.png").createImage();

	public static final Image MEDIUM_SEVERITY = Activator.getImageDescriptor("/icons/severity-medium.png")
			.createImage();

	public static final Image LOW_SEVERITY = Activator.getImageDescriptor("/icons/severity-low.png").createImage();

	@Inject
	IWorkbench workbench;

	private TreeViewer viewer;
	private Action scanWorkspace, openPrefPage, abortScanning;
	private final DisplayModel rootModel;
	private final static Shell SHELL = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

	private static final String RUNNING = "Scanning your project...";
	private static final String ABORTING = "Aborting scan...";

	private List<Action> monitorActions = new ArrayList<>();

	private boolean alreadyRunning = false;

	public SnykView() {
		rootModel = new DisplayModel();
		DisplayModel init = new DisplayModel();
		init.description = "";
		rootModel.children.add(init);
	}

	@Override
	public void createPartControl(Composite parent) {
		createViewer(parent);

		getSite().setSelectionProvider(viewer);
		makeActions();
		hookContextMenu();
		contributeToActionBars();
	}

	private void createViewer(Composite parent) {
		viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
		ColumnViewerToolTipSupport.enableFor(viewer);
		createColumns();
		final Tree table = viewer.getTree();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		viewer.setContentProvider(new TreeContentProvider());
		getSite().setSelectionProvider(viewer);

		// define layout for the viewer
		GridData gridData = new GridData();
		gridData.verticalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		gridData.grabExcessHorizontalSpace = true;
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalAlignment = GridData.FILL;
		viewer.getControl().setLayoutData(gridData);
		viewer.setInput(rootModel);

	}

	private void createColumns() {
		TreeViewerColumn col = createTreeViewerColumn("Title", 400);
		ColumnProvider label = new ColumnProvider(this::findSeverityImage, model -> model.description);
		col.setLabelProvider(new LinkLabelProvider(label));

		col = createTreeViewerColumn("Dependency", 400);
		col.setLabelProvider(new ColumnTextProvider(model -> model.dependecy));

		col = createTreeViewerColumn("Package", 400);
		col.setLabelProvider(new ColumnTextProvider(model -> model.vulnPackage));

		col = createTreeViewerColumn("Fix", 400);
		col.setLabelProvider(new ColumnTextProvider(model -> model.fix));
	}

	private Image findSeverityImage(DisplayModel model) {
		String severity = model.severity;
		if (severity == null)
			return null;

		if (severity.equalsIgnoreCase("critical"))
			return CRITICAL_SEVERITY;
		if (severity.equalsIgnoreCase("high"))
			return HIGH_SEVERITY;
		if (severity.equalsIgnoreCase("medium"))
			return MEDIUM_SEVERITY;
		if (severity.equalsIgnoreCase("low"))
			return LOW_SEVERITY;

		return null;
	}

	private TreeViewerColumn createTreeViewerColumn(String title, int bound) {
		final TreeViewerColumn viewerColumn = new TreeViewerColumn(viewer, SWT.NONE);
		final TreeColumn column = viewerColumn.getColumn();
		column.setText(title);
		column.setWidth(bound);
		column.setResizable(true);
		column.setMoveable(true);
		return viewerColumn;
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(SnykView.this::fillContextMenu);
		Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);

		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillContextMenu(IMenuManager manager) {
		ITreeSelection selection = viewer.getStructuredSelection();
		DisplayModel selected = (DisplayModel) selection.getFirstElement();
		monitorActions = new ArrayList<>();
		if (selected != null && selected.projectName != null) {
			manager.add(rerunProjectAction(selected.projectName));
			manager.add(monitorAction(selected.projectName));
		}
		if (selected != null && selected.id != null) {
			manager.add(ignoreAction(selected.id, selected.iProject));
		}
		manager.add(scanWorkspace);
		manager.add(new Separator());
		manager.add(openPrefPage);

	}

	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(scanWorkspace);
		manager.add(abortScanning);
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

		scanWorkspace = new Action() {
			@Override
			public void run() {
				if (alreadyRunning)
					return;
				scanWorkspace.setEnabled(false);
				abortScanning.setEnabled(true);
				showMessage(RUNNING);

				CompletableFuture.runAsync(() -> {
					SnykExtendedLanguageClient.getInstance().triggerScan(null);
					alreadyRunning = true;
					List<DisplayModel> scanResult = DataProvider.INSTANCE.scanWorkspace();
					rootModel.children.clear();
					rootModel.children.addAll(scanResult);
					viewer.getTree().getDisplay().asyncExec(() -> viewer.refresh());
					scanWorkspace.setEnabled(true);
					alreadyRunning = false;
				});

			}
		};

		scanWorkspace.setText("Snyk Test");
		scanWorkspace.setToolTipText("Snyk Test");
		scanWorkspace.setImageDescriptor(
				Activator.getImageDescriptor("platform:/plugin/org.eclipse.ui.browser/icons/clcl16/nav_go.png"));

		enableScanBasedOnConfig();

		abortScanning = new Action() {
			@Override
			public void run() {
				showMessage(ABORTING);
				DataProvider.abort.set(true);
				abortScanning.setEnabled(false);
			}
		};
		abortScanning.setImageDescriptor(
				Activator.getImageDescriptor("platform:/plugin/org.eclipse.ui.browser/icons/clcl16/nav_stop.png"));
		abortScanning.setEnabled(false);
	}

	private static Shell getShell() {
		var activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null)
			return SHELL;
		return activeWorkbenchWindow.getShell();
	}

	public void enableScanBasedOnConfig() {
		try {
			boolean cliFound = getCliFile().exists();
			boolean tokenFound = !Preferences.getInstance().getAuthToken().isBlank()
					|| System.getenv().containsKey("SNYK_TOKEN");

			scanWorkspace.setEnabled(cliFound && tokenFound);
			String init = "Click play to run Snyk Test";
			String msg = "";
			if (!cliFound) {
				msg = "No Snyk CLI found. Please place a CLI file in " + getCliFile().getAbsolutePath() + ". ";
			}
			if (!tokenFound) {
				msg += "No token found. Please add a Snyk Token in Snyk Preferences.";
			}
			if (scanWorkspace.isEnabled()) {
				msg = init;
			}
			showMessage(msg);
		} catch (Exception exception) {
			SnykLogger.logError(exception);
		}
	}

	public void testProject(String projectName) {
		if (alreadyRunning)
			return;
		scanWorkspace.setEnabled(false);
		abortScanning.setEnabled(true);
		showMessage(RUNNING);

		CompletableFuture.runAsync(() -> {
			alreadyRunning = true;
			List<DisplayModel> scanResult = DataProvider.INSTANCE.scanProject(projectName);
			rootModel.children.clear();
			rootModel.children.addAll(scanResult);
			viewer.getTree().getDisplay().asyncExec(() -> viewer.refresh());
			scanWorkspace.setEnabled(true);
			alreadyRunning = false;
		});
	}

	private Action rerunProjectAction(String projectName) {
		Action action = new Action() {
			@Override
			public void run() {
				testProject(projectName);
			}
		};

		action.setText("Snyk Test " + projectName);
		return action;
	}

	private Action monitorAction(String projectName) {
		Action action = new Action() {
			@Override
			public void run() {
				if (alreadyRunning)
					return;
				MessageDialog.openInformation(getShell(), "Snyk monitor",
						"Snyk monitor for project " + projectName + " in progress...");
				CompletableFuture
						.runAsync(() -> handleMonitorOutput(DataProvider.INSTANCE.monitorProject(projectName)));
			}
		};
		action.setText("Snyk monitor " + projectName);
		monitorActions.add(action);
		return action;
	}

	private Action ignoreAction(String snykId, IProject project) {
		Action action = new Action() {
			@Override
			public void run() {
				DataProvider.INSTANCE.ignoreIssue(snykId, project);
			}
		};

		action.setText("Ignore this issue");
		return action;
	}

	private static void executeInUIThread(Runnable runnable) {
		getShell().getDisplay().asyncExec(runnable);
	}

	private void handleMonitorOutput(MonitorResult result) {
		alreadyRunning = true;
		var shell = getShell();
		if (result.hasError())
			executeInUIThread(() -> MessageDialog.openError(shell, "Snyk Monitor Failed", result.getError()));
		else
			shell.getDisplay().asyncExec(() -> {
				LinkDialog dialog = new LinkDialog(shell, "Snyk Monitor Succesful",
						"Monitoring " + result.getPath() + " \n\nExplore this snapshot at: ", result.getUri());
				dialog.open();
			});
		alreadyRunning = false;

	}

	public void showMessage(String message) {
		executeInUIThread(() -> {
			rootModel.children.clear();
			rootModel.children.add(DataProvider.INSTANCE.message(message));
			viewer.refresh();
			monitorActions.forEach(act -> act.setEnabled(true));
		});
	}

	public void toggleRunActionEnablement() {
		getShell().getDisplay().asyncExec(this::enableScanBasedOnConfig);
	}

	public void disableRunAbortActions() {
		executeInUIThread(() -> {
			this.scanWorkspace.setEnabled(false);
			this.abortScanning.setEnabled(false);
		});
	}

	public static void displayMessage(String message) {
		executeInUIThread(() -> {
			try {
				getInstance().showMessage(message);
			} catch (PartInitException partInitException) {
				partInitException.printStackTrace();
			}
		});
	}

	public static SnykView getInstance() throws PartInitException {
		return (SnykView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView(ID);
	}

	@Override
	public void setFocus() {
		getShell().getDisplay().asyncExec(() -> {
			if (!viewer.getControl().isDisposed())
				viewer.getControl().setFocus();
		});
	}
}
