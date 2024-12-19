package io.snyk.eclipse.plugin.views.snyktoolview.handlers;

public interface IHandlerCommands {
	String WORKSPACE_SCAN = "io.snyk.eclipse.plugin.commands.snykWorkspaceScan";
	String WORKSPACE_FOLDER_SCAN = "io.snyk.eclipse.plugin.commands.execute";
	String STOP_SCAN = "io.snyk.eclipse.plugin.commands.snykStopScan";
	String COLLAPSE_ALL_TREE_NODES = "io.snyk.eclipse.plugin.commands.TreeCollapse";
	String EXPAND_ALL_TREE_NODES = "io.snyk.eclipse.plugin.commands.TreeExpand";
	String EMPTY_CACHE = "io.snyk.eclipse.plugin.commands.snykEmptyScanResults";
	String OPEN_PREFERENCES = "io.snyk.eclipse.plugin.properties.preferencespage";

	String SHOW_CRITICAL = "io.snyk.eclipse.plugin.commands.snykFilterCritical";
	String SHOW_HIGH = "io.snyk.eclipse.plugin.commands.snykFilterHigh";
	String SHOW_MEDIUM = "io.snyk.eclipse.plugin.commands.snykFilterMedium";
	String SHOW_LOW = "io.snyk.eclipse.plugin.commands.snykFilterLow";
	String SHOW_ALL_SEVERITIES = "io.snyk.eclipse.plugin.commands.snykShowAllSeverities";

	String ENABLE_OSS = "io.snyk.eclipse.plugin.commands.enableOSS";
	String ENABLE_CODE_SECURITY = "io.snyk.eclipse.plugin.commands.enableCodeSecurity";
	String ENABLE_CODE_QUALITY = "io.snyk.eclipse.plugin.commands.enableCodeQuality";
	String ENABLE_IAC = "io.snyk.eclipse.plugin.commands.enableIAC";
	String SHOW_ALL_PRODUCTS = "io.snyk.eclipse.plugin.command.snykShowAllProducts";

	String ENABLE_DELTA = "io.snyk.eclipse.plugin.commands.snykFilterNetNewIssues";

	String IGNORES_SHOW_OPEN = "io.snyk.eclipse.plugin.commands.snykShowOpenIgnored";
	String IGNORES_SHOW_IGNORED = "io.snyk.eclipse.plugin.commands.snykShowIgnored";

	String SHOW_ONLY_FIXABLE = "io.snyk.eclipse.plugin.commands.snykFilterFixableIssues";
	
	String getCommandByPref(String pref);
}
