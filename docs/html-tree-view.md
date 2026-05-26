# HTML Tree View

The Snyk tool view can render a fully-formed HTML tree pushed by the Language Server (LS) instead of the legacy SWT `TreeViewer`. The feature is gated behind a preference and defaults **off**.

## How it works

```
snyk-ls  ──$/snyk.treeView──►  SnykExtendedLanguageClient
                                         │
                               SnykToolView.updateTreeViewHtml(html)
                                         │
                              ┌──────────▼───────────────┐
                              │  TreeViewBrowserHandler  │
                              │  SWT Browser (Edge on    │
                              │  Windows, SWT.NONE else) │
                              └──────────────────────────┘
```

1. **LS pushes** a `$/snyk.treeView` notification with `{ treeViewHtml }`.
2. `SnykExtendedLanguageClient.snykTreeView` receives it and calls `toolView.updateTreeViewHtml(html)` if `Preferences.USE_HTML_TREE_VIEW` is `true` and the payload is non-null.
3. `SnykToolView` dispatches to the SWT thread and calls `TreeViewBrowserHandler.setBrowserText(html)`.
4. `TreeViewBrowserHandler` calls `BaseHtmlProvider.replaceCssVariables(html)` to substitute Eclipse theme colors into the HTML before passing it to `Browser.setText`.

### Pending-HTML buffer

`SnykToolView` declares `treeBrowserHandler` as `volatile` and holds an `AtomicReference<String> pendingHtml`. If a `$/snyk.treeView` notification arrives before `treeBrowserHandler` is initialized (it is assigned in `createPartControl`), the HTML is stored in `pendingHtml` and drained immediately after `treeBrowserHandler` is created.

## CSS theme substitution

The LS HTML template contains placeholder markers (`${ideStyle}` or `<style nonce="ideNonce" data-ide-style></style>`) and LS-specific CSS custom properties such as `var(--text-color)`, `var(--background-color)`, `var(--button-background-color)`, etc. `BaseHtmlProvider.replaceCssVariables(html)` substitutes these with concrete hex values read from the Eclipse CSS theming API at render time, so the tree automatically matches the active light/dark/high-contrast theme.

## JS bridge — `window.__ideExecuteCommand__`

`ExecuteCommandBridge.install(browser)` wires up the two-way command bridge:

- **JS → Java**: `window.__ideExecuteCommand__(command, args, callback)` — called by the LS tree JavaScript for commands such as `snyk.navigateToRange`, `snyk.setNodeExpanded`, `snyk.toggleTreeFilter`, `snyk.updateFolderConfig`, and `snyk.showScanErrorDetails` (these are LS-side contracts; see snyk-ls for the authoritative list). The Java side serializes the result and invokes the optional JS callback.
- The SWT native function is `__ideExecuteCommandBridge__`; `window.__ideExecuteCommand__` is the JS-visible wrapper injected into the page by `ExecuteCommandBridge.injectScript`.
- Only commands in the `snyk.*` namespace are dispatched; others are silently dropped.
- `args` is serialized with `JSON.stringify` client-side and deserialized with Jackson server-side.
- The callback registry (`window.__ideCallbacks__`) is idempotent across page reloads.

### `window.__selectTreeNode__(issueId)`

Called **from Java to JS** via `Browser.evaluate(...)`. Implemented by the LS tree JavaScript; when invoked it highlights the matching tree node without triggering a new navigation event.

`TreeViewBrowserHandler.selectNode(String issueId)` wraps the call with full JS string escaping (backslash, single-quote, newline, carriage-return, U+2028, U+2029).

## Selection sync

When the user selects an `IssueTreeNode` in the legacy `TreeViewer`, the `ISelectionChangedListener` in `SnykToolView` (lines 143–149) calls `treeBrowserHandler.selectNode(issue.id())` so the HTML tree highlights the same node. `SnykToolView.selectTreeNode` also drives this indirectly: it updates the `TreeViewer` selection, which fires the listener.

## Enabling the feature

The HTML tree is **disabled by default**. The preference is read once when the Snyk tool view is opened (`createPartControl`) and controls which widget is visible. Changing the preference while the view is open requires closing and re-opening the view.

To enable:

1. Open **Window → Preferences → Snyk** (or **Eclipse → Preferences → Snyk** on macOS).
2. Check **Use HTML tree view (experimental)**.
3. Close and re-open the Snyk tool view.

Or programmatically (takes effect on next view open):

```java
Preferences.getInstance().store(Preferences.USE_HTML_TREE_VIEW, "true");
```

## Key classes

| Class | Package | Role |
|---|---|---|
| `TreeViewBrowserHandler` | `views/snyktoolview` | Wraps SWT `Browser`; `setBrowserText`, `selectNode`, `initialize` |
| `ExecuteCommandBridge` | `html` | Registers `__ideExecuteCommandBridge__` SWT BrowserFunction; injects `window.__ideExecuteCommand__` JS wrapper |
| `BaseHtmlProvider` | `html` | `replaceCssVariables(html)` — substitutes LS CSS custom properties with Eclipse theme colors |
| `TreeViewParams` | `protocolextension/messageObjects` | DTO for `$/snyk.treeView` payload (`treeViewHtml`) |
| `SnykExtendedLanguageClient` | `protocolextension` | `@JsonNotification` handler; dispatches to `ISnykToolView` |
