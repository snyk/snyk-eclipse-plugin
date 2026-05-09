---

**Findings**

---

1. **[HIGH] `treeBrowserHandler` is non-volatile but read from multiple background threads — data race with no JMM guarantee**

   `treeBrowserHandler` is a plain instance field (line 72). It is written once on the SWT UI thread in `createPartControl` (line 110). It is then read without synchronization from background threads in three places:

   - `updateTreeViewHtml` (line 566), called from `CompletableFuture.runAsync` (SnykExtendedLanguageClient.java:449)
   - `selectTreeNode(Issue, String)` (line 537), called from `CompletableFuture.supplyAsync` (SnykExtendedLanguageClient.java:483)
   - `selectTreeNode(String)` (line 575)

   The JMM requires a happens-before edge for a write to be visible to another thread. There is none here: no `volatile`, no `synchronized`, no shared monitor. On ARM (Apple Silicon — every modern Mac), the weak memory model allows a background thread to observe a stale null for `treeBrowserHandler` long after the UI thread has written it. The `volatile` was correctly added to `pendingHtml` but was not applied to `treeBrowserHandler`, leaving the primary guard unprotected. Making `treeBrowserHandler` `volatile` is the minimal fix; it enables safe publication.

---

2. **[HIGH] TOCTOU between `updateTreeViewHtml`'s null check and `createPartControl`'s drain permanently loses pending HTML**

   The "pending buffer" design relies on two separate variables (`treeBrowserHandler` and `pendingHtml`) being checked atomically, which they are not. The following interleaving is possible:

   1. UI thread: `createPartControl` writes `treeBrowserHandler` (non-volatile, no fence).
   2. UI thread: reads `pendingHtml` — sees null, no drain scheduled.
   3. BG thread: `updateTreeViewHtml` reads `treeBrowserHandler` — sees stale null (step 1 not yet visible).
   4. BG thread: writes `pendingHtml = html` (volatile write) and returns early.

   Now `pendingHtml` holds a value but `treeBrowserHandler` is already initialized. The drain only occurs in `createPartControl` (lines 113–117), which runs exactly once per view lifetime. It will never fire again. The pending HTML is silently and permanently lost; the tree browser remains blank until the next scan notification arrives — which may not happen in the current session.

   The fix requires atomically relating the two checks: both the drain in `createPartControl` and the store-to-pending in `updateTreeViewHtml` must be `synchronized (this)` blocks, or `treeBrowserHandler` must be volatile and an explicit memory fence established before the drain reads `pendingHtml`.

---

3. **[MEDIUM] `createPartControl` drains `pendingHtml` via `asyncExec` even though it already runs on the UI thread — guarantees a blank-flash and re-introduces the TOCTOU window**

   Lines 113–117:
   ```java
   if (pendingHtml != null) {
       String html = pendingHtml;
       pendingHtml = null;                        // cleared now
       Display.getDefault().asyncExec(…);         // applied later
   }
   ```
   `createPartControl` executes on the SWT UI thread. The `asyncExec` defers `setBrowserText` to the *next* event loop iteration. Between `pendingHtml = null` and the lambda actually running, any background thread that calls `updateTreeViewHtml`, now sees `treeBrowserHandler != null` and calls `asyncExec` itself. The order in the event queue between the drain and the new update is non-deterministic. The simpler and correct approach is to call `treeBrowserHandler.setBrowserText(html)` directly on the current UI-thread call frame, removing the async gap entirely.

---

4. **[MEDIUM] `pendingHtml` is a single slot — multiple pre-init HTML deliveries silently drop all but the last**

   `updateTreeViewHtml` is called from `CompletableFuture.runAsync` for every `snykTreeView` LSP notification (SnykExtendedLanguageClient.java:449-452). During startup—before the tool view is opened—the language server may deliver several tree-view updates (e.g., product-by-product as scans complete). Each call overwrites `pendingHtml` (line 567). When `createPartControl` finally runs, it drains only the last value. Earlier, potentially more complete, HTML payloads are permanently discarded with no log, no metric, and no observable signal. The author appears to assume the final notification is always the most complete one; this is not proven.

---

5. **[LOW] JavaScript string escaping in `selectNode()` does not cover newlines or control characters — silent syntax error if issue ID contains them**

   `TreeViewBrowserHandler.java` line 37:
   ```java
   String escaped = issueId.replace("\\", "\\\\").replace("'", "\\'");
   browser.evaluate("…window.__selectTreeNode__('" + escaped + "')…");
   ```
   A single-quoted JavaScript string literal cannot contain a literal `\n` or `\r` character — the browser's JS engine throws a syntax error. The outer `if(window.__selectTreeNode__)` guard does not suppress parse errors; `browser.evaluate` returns null and swallows the failure silently. Snyk-generated issue IDs (e.g., `SNYK-JS-X-123456`, UUIDs) do not currently contain these characters, but the contract is not enforced: the method accepts any `String`, the interface exposes it publicly, and future callers or LS changes could pass IDs with embedded newlines. Minimum escaping: `issueId.replace("\n","\\n").replace("\r","\\r")` after the existing replacements. Better: use `JSONObject.quote(issueId)` or pass the ID via `browser.evaluate(script, args)` if the SWT API supports it, to avoid manual escaping entirely.
