package io.snyk.eclipse.plugin.views;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import javax.inject.Inject;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
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
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.part.ViewPart;

import io.snyk.eclipse.plugin.Activator;

public class SnykView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "io.snyk.eclipse.plugin.views.SnykView";
	public static final Image ERROR = Activator.getImageDescriptor(
            "platform:/plugin/org.eclipse.ui.views.log/icons/obj16/error_st_obj.png").createImage();
	public static final Image WARNING = Activator.getImageDescriptor(
            "platform:/plugin/org.eclipse.ui.views.log/icons/obj16/warning_st_obj.png").createImage();
	public static final Image INFO = Activator.getImageDescriptor(
            "platform:/plugin/org.eclipse.ui.views.log/icons/obj16/info_st_obj.png").createImage();


	@Inject
	IWorkbench workbench;

	private TreeViewer viewer;
	private Action scanWorkspace, openPrefPage, abortScanning;	
	private DisplayModel model;
	private Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
	
	private static final String RUNNING = "running...";
	private static final String ABORTING = "abort scanning...";
	
	public SnykView() {
		model = new DisplayModel();
		DisplayModel init = new DisplayModel();
		init.description = "Hit play to run scan";
		model.children.add(init);
	}
	

	@Override
	public void createPartControl(Composite parent) {
		createViewer(parent);

		workbench.getHelpSystem().setHelp(viewer.getControl(), "io.snyk.viewer");
		getSite().setSelectionProvider(viewer);
		makeActions();
		hookContextMenu();
		contributeToActionBars();
	}

	private void createViewer(Composite parent) {
		viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
		ColumnViewerToolTipSupport.enableFor(viewer);
		createColumns(parent, viewer);
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
		viewer.setInput(model);
		
	}

	private void createColumns(final Composite parent, final TreeViewer viewer) {
		TreeViewerColumn col = createTreeViewerColumn("Title", 400, 0);
		ColumnProvider label = new ColumnProvider(this::findSeverityImage ,model -> model.description);
		col.setLabelProvider(new LinkLabelProvider(label));

		col = createTreeViewerColumn("Dependency", 400, 1);
		col.setLabelProvider(new ColumnTextProvider(model -> model.dependecy));

		col = createTreeViewerColumn("Package", 400, 2);
		col.setLabelProvider(new ColumnTextProvider(model -> model.vulnPackage));

		col = createTreeViewerColumn("Fix", 400, 3);
		col.setLabelProvider(new ColumnTextProvider(model -> model.fix));
	}

	private Image findSeverityImage(DisplayModel model) {
		String severity = model.severity;
		if (severity == null) return null;

		if (severity.equalsIgnoreCase("high")) return ERROR;
		if (severity.equalsIgnoreCase("medium")) return WARNING;
		if (severity.equalsIgnoreCase("low")) return INFO;
		return null;
	}

	private TreeViewerColumn createTreeViewerColumn(String title, int bound, final int colNumber) {
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
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				SnykView.this.fillContextMenu(manager);
			}
		});
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
		DisplayModel selected = (DisplayModel)selection.getFirstElement();
		if (selected != null && selected.projectName != null) {
			manager.add(rerunProjectAction(selected.projectName));
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
		scanWorkspace = new Action() {
			@Override
			public void run() {
				showMessage(RUNNING);
				scanWorkspace.setEnabled(false);
				abortScanning.setEnabled(true);
				
				CompletableFuture.runAsync(()-> {
					List<DisplayModel> scanResult = DataProvider.INSTANCE.scanWorkspace();
					model.children.clear();
					model.children.addAll(scanResult);
					viewer.getTree().getDisplay().asyncExec(() -> viewer.refresh());
					scanWorkspace.setEnabled(true);
				});
				
			}
		};
		scanWorkspace.setText("Snyk test");
		scanWorkspace.setToolTipText("Snyk test");
		scanWorkspace.setImageDescriptor(
				Activator.getImageDescriptor("platform:/plugin/org.eclipse.ui.browser/icons/clcl16/nav_go.png"));
		
		openPrefPage = new Action() {
			
			@Override
			public void run() {
				
				PreferenceDialog pref = PreferencesUtil.createPreferenceDialogOn(
						shell, "io.snyk.eclipse.plugin.properties.preferencespage",  
						null, null);
				if (pref != null) pref.open();
			}
		};
		
		openPrefPage.setText("Preferences");
		
		abortScanning = new Action() {
			@Override
			public void run() {
				showMessage(ABORTING);
				DataProvider.INSTANCE.abort.set(true);
				abortScanning.setEnabled(false);
			}			
		};
		abortScanning.setImageDescriptor(
				Activator.getImageDescriptor("platform:/plugin/org.eclipse.ui.browser/icons/clcl16/nav_stop.png"));
		abortScanning.setEnabled(false);
	}
		
	private Action rerunProjectAction(String projectName) {
		Action action = new Action() {
			public void run() {
				showMessage(RUNNING);
				scanWorkspace.setEnabled(false);
				abortScanning.setEnabled(true);
				
				CompletableFuture.runAsync(()-> {
					model.children.clear();
					model.children.addAll(DataProvider.INSTANCE.scanProject(projectName));
					viewer.getTree().getDisplay().asyncExec(() -> viewer.refresh());
					scanWorkspace.setEnabled(true);
				});
				
			}
		};
		
		action.setText("Snyk test " + projectName );
		return action;
	}
	
	public void showMessage(String message) {
		model.children.clear();
		model.children.add(DataProvider.INSTANCE.message(message));
		viewer.refresh();
	}
	

	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}
}
