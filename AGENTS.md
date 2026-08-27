## Project Overview

`snyk/snyk-eclipse-plugin` is an Eclipse plugin (`io.snyk.eclipse.plugin`) that integrates Snyk scanning (Open Source, Code, IaC and Secrets) into the Eclipse IDE. It downloads and manages the `snyk-ls` language server binary locally and speaks LSP to it over stdio using LSP4E (`org.eclipse.lsp4e`) and LSP4J. Scan results and custom protocol extensions surface findings in a dedicated "Snyk" tool view rendered as HTML/tree UI inside SWT browsers. Built with Maven/Tycho against an Eclipse target platform (`target-platform/target-platform.target`).

## Build & Development Commands

```bash
./mvnw clean verify -DtrimStackTrace=false          # full multi-module Tycho build + tests (CI default)
./mvnw clean verify -P sign -DtrimStackTrace=false  # release build on main, signs with keystore.jks
./mvnw package                                      # fetch external jars into plugin/target/dependency (for PDE dev setup)
```

Tests run via `tycho-surefire-plugin` (module `tests/`, JUnit 5) as part of `verify` — there is no separate test-only target. `maven-pmd-plugin` runs `check`/`cpd-check` in `verify` for `plugin/` (ruleset `plugin/src/main/resources/pmd-ruleset.xml`); CI additionally runs `pmd-github-action` as a hard lint gate before the Maven build, so keep PMD clean.

## Architecture

Root `pom.xml` declares five Tycho modules, built in order:
- `target-platform/` — `target-platform.target`, pinning the Eclipse Platform + LSP4E/LSP4J versions the build resolves against.
- `plugin/` (`io.snyk.eclipse.plugin`) — the plugin code, under `plugin/src/main/java/io/snyk/`:
  - `eclipse/plugin/` — `Activator.java`, `SnykStartup.java` (bundle lifecycle/startup).
  - `eclipse/plugin/views/snyktoolview/` — the Snyk tool view (`SnykToolView`, `BrowserHandler`, `TreeViewBrowserHandler`) and `handlers/` (menu commands).
  - `eclipse/plugin/html/` — HTML providers rendering scan results into the embedded browser.
  - `eclipse/plugin/preferences/`, `properties/` — preference pages and per-project/folder settings.
  - `eclipse/plugin/wizards/`, `analytics/`, `domain/`, `utils/` — setup wizard, analytics events, domain model, shared utilities.
  - `languageserver/` — LSP4E integration: `SnykLanguageServer`, `LsRuntimeEnvironment`, `LsConfigurationUpdater`, `WorkspaceFolderChangeTracker`, `CommandHandler`.
  - `languageserver/download/` — binary download/verification of the `snyk-ls`/CLI executable.
  - `languageserver/protocolextension/` — custom LSP protocol handling (`SnykExtendedLanguageClient`, `ProgressManager`) and `messageObjects/scanResults/` DTOs mapping LS JSON payloads.
- `feature/` — `feature.xml`/`category.xml` packaging the plugin into an Eclipse feature.
- `tests/` (`io.snyk.eclipse.plugin.tests`) — JUnit 5 test bundle, mirrors `plugin/`'s package structure.
- `update-site/` — `eclipse-repository` module producing the p2 update site.

## Conventions

- Test classes are named `<ClassUnderTest>Test.java`, placed in a mirrored package path under `tests/src/test/java` (not co-located with source).
- Language-server-related tests extend the shared base class `LsBaseTest`.
- Mocking via Mockito (`mockito-inline`, `mockito-junit-jupiter`) plus Instancio for test-data generation; JUnit 5 (Jupiter) throughout, not JUnit 4.
- Java package root splits into `io.snyk.eclipse.plugin.*` (Eclipse-specific UI/wiring) and `io.snyk.languageserver.*` (LSP/protocol logic) — a deliberate separation between IDE glue and language-server integration.

## Development Workflow

- Use TDD: write/update tests before implementation, iterate until green.
- For non-trivial work, write an implementation plan first and get confirmation before starting; never commit the plan.
- Make the minimum change needed — don't refactor or optimize beyond the stated goal. Comment on *why*, not *what*.
- Use Mockito for mocking and reuse existing mocks rather than writing new ones.
- After changing `.java` files, run `./mvnw verify` and fix any issues before continuing. Never disable a test to get past this — only a human may do that.
- Run Snyk SCA/Code scans against the project's absolute path after each edit and before committing; fix real findings, don't touch test fixtures.
- Never skip commit hooks (no `--no-verify`). Use atomic, conventional-commit-style commits; if a Jira ID (`XXX-XXXX`) appears in the branch name, append it to the subject.
- This is not a library: delete unused files instead of deprecating them.
