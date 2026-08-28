## Project Overview

`snyk/snyk-eclipse-plugin` is an Eclipse plugin (`io.snyk.eclipse.plugin`) that integrates Snyk scanning (Open Source, Code, IaC and Secrets) into the Eclipse IDE. It downloads and manages the `snyk-ls` language server binary locally and speaks LSP to it over stdio using LSP4E (`org.eclipse.lsp4e`) and LSP4J. Scan results and custom protocol extensions surface findings in a dedicated "Snyk" tool view rendered as HTML/tree UI inside SWT browsers. Built with Maven/Tycho against an Eclipse target platform (`target-platform/target-platform.target`).

## Build & Development Commands

```bash
./mvnw clean verify -DtrimStackTrace=false          # full multi-module Tycho build + tests (CI default)
./mvnw clean verify -P sign -DtrimStackTrace=false  # release build on main, signs with keystore.jks
./mvnw package                                      # fetch external jars into plugin/target/dependency (for PDE dev setup)
```

Tests run via `tycho-surefire-plugin` (module `tests/`, JUnit 5) as part of `verify`; there is no separate test-only target. `maven-pmd-plugin` runs `check`/`cpd-check` in `verify` for `plugin/` (ruleset `plugin/src/main/resources/pmd-ruleset.xml`); CI additionally runs `pmd-github-action` as a hard lint gate before the Maven build, so keep PMD clean.

## Architecture

Root `pom.xml` declares five Tycho modules, built in order:
- `target-platform/` contains `target-platform.target`, pinning the Eclipse Platform + LSP4E/LSP4J versions the build resolves against.
- `plugin/` (`io.snyk.eclipse.plugin`) is the plugin code, under `plugin/src/main/java/io/snyk/`:
  - `eclipse/plugin/` has `Activator.java`, `SnykStartup.java` (bundle lifecycle/startup).
  - `eclipse/plugin/views/snyktoolview/` has the Snyk tool view (`SnykToolView`, `BrowserHandler`, `TreeViewBrowserHandler`) and `handlers/` (menu commands).
  - `eclipse/plugin/html/` has HTML providers rendering scan results into the embedded browser.
  - `eclipse/plugin/preferences/`, `properties/`: preference pages and per-project/folder settings.
  - `eclipse/plugin/wizards/`, `analytics/`, `domain/`, `utils/`: setup wizard, analytics events, domain model, shared utilities.
  - `languageserver/` handles LSP4E integration: `SnykLanguageServer`, `LsRuntimeEnvironment`, `LsConfigurationUpdater`, `WorkspaceFolderChangeTracker`, `CommandHandler`.
  - `languageserver/download/` handles binary download/verification of the `snyk-ls`/CLI executable.
  - `languageserver/protocolextension/` has custom LSP protocol handling (`SnykExtendedLanguageClient`, `ProgressManager`) and `messageObjects/scanResults/` DTOs mapping LS JSON payloads.
- `feature/` has `feature.xml`/`category.xml` packaging the plugin into an Eclipse feature.
- `tests/` (`io.snyk.eclipse.plugin.tests`) is the JUnit 5 test bundle, mirroring `plugin/`'s package structure.
- `update-site/` is the `eclipse-repository` module producing the p2 update site.

## Conventions

- Test classes are named `<ClassUnderTest>Test.java`, placed in a mirrored package path under `tests/src/test/java` (not co-located with source).
- Language-server-related tests extend the shared base class `LsBaseTest`.
- Mocking via Mockito (`mockito-inline`, `mockito-junit-jupiter`) plus Instancio for test-data generation; JUnit 5 (Jupiter) throughout, not JUnit 4.
- Java package root splits into `io.snyk.eclipse.plugin.*` (Eclipse-specific UI/wiring) and `io.snyk.languageserver.*` (LSP/protocol logic), a deliberate separation between IDE glue and language-server integration.

## Development Workflow

- Never commit an implementation plan to the repo.
- Use Mockito for mocking and reuse existing mocks rather than writing new ones.
- This is not a library: delete unused files instead of deprecating them.
- After changing `.java` files, run `./mvnw verify` and fix any issues before continuing.
- Run Snyk SCA/Code scans against the project's absolute path before committing and after `pom.xml` changes; fix real findings, don't touch test fixtures.
- Before each commit, check for and address feedback from the PR review bot (snyk-pr-review-bot) on any open PR.
- Never skip commit hooks (no `--no-verify`). Use atomic, conventional-commit-style commits; if a Jira ID (`XXX-XXXX`) appears in the branch name, append it to the subject.
- Never push without asking first, and never force-push. Regularly fetch `main` and offer to merge it into the working branch.
- After pushing, offer to open a draft PR using `.github/pull_request_template.md` (or update the existing PR description) with a title/description generated from the diff against `main`. Per `CONTRIBUTING.md`, a change applicable to the other Snyk IDE plugins (vscode-extension, snyk-intellij-plugin, snyk-visual-studio-plugin) should get matching PRs opened there too, since releases are usually coordinated.
- Keep `./docs` up to date; document tested scenarios and add Mermaid diagrams for new flows.
