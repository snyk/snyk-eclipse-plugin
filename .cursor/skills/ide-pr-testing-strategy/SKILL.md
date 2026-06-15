---
name: ide-pr-testing-strategy
description: >-
  Reviews IDE plugin PRs against the Team IDE Testing Strategy (LDX-Sync / config
  sync) and suggests automated tests, CI gates, and manual QA. Use when reviewing
  a pull request, asking for test coverage improvements, QA plan for IDE-1652, or
  mapping changes to the Confluence Testing Strategy page.
---

# IDE PR Testing Strategy Review

Apply the [IDE Testing Strategy](https://snyksec.atlassian.net/wiki/spaces/IDE/pages/4612227128/Testing+Strategy) to a PR under review. Output **actionable test and QA suggestions** the author can land in the same PR (or the next stack PR).

**Not a substitute for** `review-tests` (test quality) or `pr-review-bot` (logic/security). Use this skill when the question is: *does this PR satisfy our release testing strategy, and what should we add?*

## Inputs

Resolve in order:

1. **PR scope** — URL, branch name, or `gh pr diff`. If user gives a stack position (e.g. "2/3 wiring"), use [reference.md](reference.md) stack table.
2. **Strategy source** — prefer, in order:
   - Confluence: page `4612227128` in space IDE (`getConfluencePage` with cloudId for `snyksec.atlassian.net`)
   - Repo doc: `docs/IDE-1652-testing-qa-plan.md` (Eclipse IDE-1652)
   - This skill: [reference.md](reference.md)
3. **Changed files** — `git diff` base...HEAD or PR files list.

## Workflow

Copy and track:

```
Progress:
- [ ] Step 1: Classify PR (repo, layer, stack role)
- [ ] Step 2: Map diff → strategy sections (§1–§8)
- [ ] Step 3: Inventory existing tests touched / missing
- [ ] Step 4: Cross-IDE parity check (VS Code / IntelliJ patterns)
- [ ] Step 5: Emit recommendations (automated / CI / manual)
```

### Step 1: Classify the PR

| Signal | Classification |
|--------|----------------|
| `LsSettingsRegistry`, protocol POJOs only | **Foundation** (Eclipse #394-like) |
| `$/snyk.configuration`, `LsConfigurationUpdater`, `FolderConfigSettings` | **Wiring** (#395-like) |
| `HTMLSettingsPreferencePage`, `settings-fallback.html` | **HTML migration** (#396-like) |
| Deletions only, native UI / legacy POJOs | **Cleanup** (#393-like) — verify base branch and compile |
| `snyk-ls` config / LDX | **LS-primary** — link LS tests; IDE adapter only if touched |
| VS Code / IntelliJ settings webview | **IDE adapter** — integration + unit on persistence |

**Stack rule (IDE-1652):** review order 394 → 395 → 396 → 393. Cleanup PR must target the HTML migration branch, not `main`.

### Step 2: Map diff → strategy sections

For each changed production file, assign primary section(s):

| § | Topic | IDE must prove (automate if possible) |
|---|--------|--------------------------------------|
| §1 | Persistence & migration | Upgrade keeps prefs; folder config round-trip; explicit-changed on real diffs |
| §2 | Context-aware config | Per-folder settings reach LS outbound payload (defer multi-project to LS) |
| §3 | Locked / origin UI | Wire format for `isLocked` / origin — manual UI until rendered |
| §4 | Lifecycle | Init options shape; re-auth refetch; offline cache (LS-heavy) |
| §5 | Scan parity | Save → outbound `LspConfigurationParam` golden JSON |
| §6 | External MDM | LS tests + manual exploratory |
| §7 | Errors & injection | Invalid config graceful; token not inbound-persisted; XSS on HTML subs |
| §8 | Performance | Manual checklist only |

Tag each gap: **LS-Auto** | **IDE-Auto** | **Manual** | **Planned** (see [reference.md](reference.md)).

### Step 3: Inventory tests

Search the repo for tests related to changed types:

- **Eclipse:** `tests/src/test/java/**/*Test.java`, run pattern `./mvnw test -pl plugin,tests`
- **VS Code:** `src/test/unit/**`, `src/test/integration/**`, `npm run test:unit`
- **IntelliJ:** `src/test/kotlin/**`, `./gradlew test`
- **snyk-ls:** `*_test.go`, `INTEG_TESTS=1`, `SMOKE_TESTS=1`

For each strategy row touched by the diff, answer:

- Is there a test that would **fail** if this PR regressed?
- Does the test assert **values** or only `changed` / non-null?
- Was a test **deleted** without replacement (red flag for #393-like PRs)?

### Step 4: Cross-IDE parity

If the change touches config save/load or LSP config:

| Eclipse | VS Code | IntelliJ |
|---------|---------|----------|
| `HTMLSettingsPreferencePageTest` | `configurationPersistenceService.test.ts` | `SaveConfigHandlerTest` |
| `SnykExtendedLanguageClientTest` | `configuration.test.ts` (integration) | `SnykProjectSettingsConfigurableTest` |
| `LsConfigurationUpdaterTest` | `serverSettingsToLspConfigurationParam.test.ts` | folder config tests |

Suggest aligning JSON fixture shapes (snake_case folder keys, skip inbound `token`, explicit-changed semantics).

### Step 5: Recommendations

Prioritize:

- **P0** — merge blocker: missing test for known Critical review pattern (see [reference.md](reference.md) traceability table)
- **P1** — should add in same PR: closes strategy gap marked "No" on Confluence
- **P2** — follow-up: CI gate, shared fixtures, Confluence link updates
- **Manual** — short exploratory bullet for beta sheet only

Each automated suggestion MUST include:

- Suggested **test class and method name** (e.g. `parseAndSaveConfig_folderConfigRoundTrip_preferredOrg`)
- **Strategy §** and **Confluence scenario** (plain language)
- **Fixture** hint (inline JSON snippet or `tests/resources/config-fixtures/...`)

## Output format

Use this template for the user (PR comment, review doc, or chat):

```markdown
# Testing strategy review — [PR title / URL]

## PR classification
- **Repo:** …
- **Role:** Foundation | Wiring | HTML | Cleanup | LS | Other
- **Stack:** … (if applicable)

## Strategy coverage

| § | Scenario (short) | Current coverage | Gap |
|---|------------------|------------------|-----|
| §1 | … | LS-Auto / IDE-Auto / None | … |

## Recommended automated tests (add in this PR)

### P0
1. **`ClassName#methodName`** — …
   - Fixture: `{ ... }`
   - Strategy: §… — …

### P1
…

## CI / repo improvements
- …

## Manual QA (keep out of CI)
- [ ] …

## Confluence updates
Suggest marking row "…" as **IDE-Auto** with link to `ClassName#methodName` after merge.

## Verdict
**Testing: READY | NEEDS TESTS | NEEDS MANUAL PLAN**

One sentence rationale.
```

## Eclipse IDE-1652 quick triggers

If the diff includes any of these, cross-check [reference.md](reference.md) **IDE-1652 matrix**:

- `LsSettingsRegistry`, `LspConfigurationParam`, `ConfigSetting`
- `persistGlobalSettings`, `markExplicitlyChanged`, `EXPLICIT_CHANGES`
- `HTMLSettingsPreferencePage`, `loadFallbackHtml`, `settings-fallback.html`
- `FolderConfigSettings`, `processFolderConfig`
- Deletion of `PreferencesPage`, `IdeConfigData`, factory tests

## Tools

| Tool | Use |
|------|-----|
| `gh pr diff`, `gh pr view` | PR files and description |
| Confluence MCP | Fresh strategy page body |
| `Grep` / `Glob` | Find tests and peers |
| `Read` | `docs/IDE-1652-testing-qa-plan.md` in snyk-eclipse-plugin |

## Additional resources

- Strategy sections and IDE-1652 P0 tests: [reference.md](reference.md)
- Full Eclipse QA plan (shareable doc): `snyk-eclipse-plugin/docs/IDE-1652-testing-qa-plan.md`
