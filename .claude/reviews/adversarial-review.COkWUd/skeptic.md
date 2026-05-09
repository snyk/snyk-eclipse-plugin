---

1. **[HIGH] `resolveFontSize` emits points as pixels** — `EclipseThemeCssProvider.java:76`

   `FontData.getHeight()` is documented to return the size in **points**, not pixels. The code appends `"px"` directly: `getHeight() + "px"`. On a standard 96 DPI screen 1pt ≈ 1.333px, so a Windows system font of 9pt becomes `"9px"` (should be ~12px). `BaseHtmlProvider.getRelativeFontSize()` explicitly handles this with `pxToPtMultiplier = 72.0/96.0`; this code ignores the same problem. The correct CSS unit here is `"pt"`, or the value needs the same conversion `BaseHtmlProvider` already does. The test suite passes `null` for Display and checks only the `"13px"` fallback, so this bug passes CI silently.

2. **[HIGH] `toHex()` strips alpha; two live-path values lose their transparency entirely** — `EclipseThemeCssProvider.java:10,19,21`

   `toHex()` produces only `#rrggbb`. But two variables are semantically required to have an alpha channel:
   - `--vscode-widget-shadow` fallback is `"#00000026"` (semi-transparent). When a real Display is present, `SWT.COLOR_WIDGET_DARK_SHADOW` resolves to `"#000000"` — a fully opaque black. In any theme the shadow becomes a solid black box.
   - `--vscode-list-hoverBackground` fallback is `"#0066cc1f"` (~12% opacity tint). The live path resolves `SWT.COLOR_LIST_SELECTION` to the full selection color. Hovering over any row renders identically to a selected row.

   These are not subtle rounding errors; they produce visually broken output on any real Eclipse display. The fix for shadow is `rgba()` or a dedicated opacity-bearing approach; the live path cannot represent transparency via a 6-char hex.

3. **[HIGH] Multi-word font family names are not quoted in CSS** — `EclipseThemeCssProvider.java:31,65`

   `resolveFontFamily` returns the raw `FontData.getName()` value — `"Segoe UI"` on Windows, `"Ubuntu"` on Linux, etc. This is emitted verbatim:
   ```
   --vscode-font-family:Segoe UI;
   ```
   CSS custom property values are stored as-is, so wherever `var(--vscode-font-family)` is consumed in `font-family:`, the parser sees two tokens (`Segoe` and `UI`), neither of which is a valid family. The font silently falls back to the browser default. On macOS, many system font identifiers are single words, so this is platform-specific — "works on my machine" if the developer is on macOS. The fix is to wrap the name in CSS quotes: `"\"" + name + "\""`.

4. **[MEDIUM] Four semantically distinct CSS variables all resolve to the same `SWT.COLOR_LIST_SELECTION`** — `EclipseThemeCssProvider.java:20–22,26`

   `focusBorder`, `listHoverBg`, `listSelectionBg`, and `buttonBg` all call `resolveColor(display, SWT.COLOR_LIST_SELECTION, ...)`. Setting aside the alpha issue in finding #2, this means:
   - Hover state is visually identical to selection state — every hovered item looks selected.
   - Focus ring color equals the selection fill color — keyboard navigation is visually ambiguous.
   - Button background equals selection fill — works only when that color is an appropriate action color, but breaks on themes where the selection color is e.g. an amber or teal accent.

   The different fallback strings (`"#0066cc1f"` for hover vs. `"#0066cc"` for selection) prove the *intent* was to produce different values. The implementation doesn't fulfill the intent.

5. **[MEDIUM] Test suite exercises only the `null`-Display fallback path; the live SWT code path is never tested** — `EclipseThemeCssProviderTest.java:41,47,53,60`

   Every `buildStyleBlock` call in the test passes `null`, which routes through the `display == null` guard and returns hardcoded fallback strings. The `resolveColor → display.getSystemColor() → toHex()` chain is never invoked by any test. The font bugs in findings #1 and #3, and the alpha loss in finding #2, all survive the full test run. The tests prove the fallback strings are concatenated correctly; they say nothing about the behavior on a real Eclipse installation. There is no integration or smoke test that constructs a real Display and asserts the output.

6. **[MEDIUM] `buildStyleBlock` is `public static` with an implicit UI-thread requirement and no enforcement** — `EclipseThemeCssProvider.java:13`

   `Display.getSystemColor()` and `Display.getSystemFont()` must be called from the SWT UI thread; calling them from a background thread throws `SWTException(ERROR_THREAD_INVALID_ACCESS)`. The public static signature `buildStyleBlock(Display display)` gives no indication of this constraint. Current callers are safe because `SnykToolView.updateTreeViewHtml` wraps the call in `asyncExec`. But `buildStyleBlock` is part of the public API of `EclipseThemeCssProvider` — a future caller on a worker thread (e.g., a background refresh) will throw, and the exception will be uncaught in a thread pool executor, leaving the tree view blank with no user-visible diagnostic. No thread assertion (e.g., `Display.getCurrent() != null`) guards the entry point.

7. **[LOW] `String.replace("</head>", ...)` replaces all occurrences** — `TreeViewBrowserHandler.java:42`

   If the LS-served HTML ever contains `</head>` inside a `<script>`, comment, or template literal, the style block is injected at every occurrence. The current `tree.html` has exactly one `</head>`, so this is dormant. If the LS upgrades the HTML to include embedded script templates or comments referencing the tag, injection silently corrupts the document in multiple places. `String.replaceFirst()` with a targeted pattern, or a proper insertion-point search, would be safer.

8. **[LOW] Unquoted font name in CSS is also an injection surface** — `EclipseThemeCssProvider.java:65`

   OS-provided font names are emitted directly into a `<style>` block with no escaping. A font name containing `}`, `;`, or `<` would corrupt the CSS rule or break out of the `<style>` tag. In practice, SWT returns sane names, but the code places unconditional trust in OS-supplied data inside an HTML context. At minimum, a `;` or `}` in a font name would silently truncate the `:root` block, suppressing all subsequent variable definitions.
