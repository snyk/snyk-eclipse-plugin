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

## Cursor Cloud specific instructions

Durable, non-obvious notes for agents running in the Cursor Cloud Linux VM. There
is no dependency-install step to repeat here: the Maven wrapper (`./mvnw`) fetches
Maven itself from `repo.maven.apache.org` on first use.

- **Build and test with JDK 17, not the VM's default JDK 21.** The target
  platform's execution environment is `JavaSE-17` and the root `pom.xml` sets
  `maven.compiler.source`/`target` to 17. Compilation and packaging succeed under
  21, but the test module does not: `tests/pom.xml` pins Mockito 4.5.1 and pins the
  inline mock maker's agent explicitly at `net.bytebuddy:byte-buddy-agent:1.14.1`,
  which predates JDK 21 support. Under 21 the agent cannot instrument classes and
  every mock-based test errors with `MockitoException: Mockito cannot mock this
  class …` — 187 errors across 20+ test classes on Temurin 21.0.6, versus 0 errors
  on Temurin 17. Point `JAVA_HOME` at a 17 before invoking Maven:
  `export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64; export PATH="$JAVA_HOME/bin:$PATH"`
  (install a Temurin 17 if none is present). Because the agent coordinate is pinned
  directly rather than inherited from Mockito, bumping Mockito alone would not lift
  this — both would need to move.
- **Commands:** `./mvnw package` builds and packages the plugin (jar under
  `plugin/target/`). `./mvnw clean verify -DtrimStackTrace=false` is the CI command
  and additionally runs the headless Tycho tests; a clean run compiles every bundle
  and the test suite passes with no failures. Do **not** pass `-P sign` — it requires
  signing certs.
- **Add `-Declipse.p2.mirrors=false`.** Tycho otherwise follows p2 mirror redirects
  out to arbitrary university and ISP mirrors that cannot be enumerated in an
  egress allowlist; the flag forces direct downloads from `download.eclipse.org`.
- **Tests run headless** under Tycho surefire, so no display is needed for the build.
- **`eclipse -version` launches the full IDE, it is not a CLI version query.** A script that
  runs it to read the version blocks on a GUI launch instead of printing and exiting. Read the
  version from the install's metadata (e.g. `readme/readme_eclipse.html` or
  `.eclipseproduct` / `configuration/config.ini` under the Eclipse home) instead.
- **Installing the built plugin into a real Eclipse is the strongest proof**, since
  building and unit tests do not show that the plugin works inside a running IDE, and
  it is achievable where the VM provides a display (cloud VMs here have run XFCE on
  `DISPLAY=:1`). Two things bite, both avoidable:
  - **Getting the matching Eclipse release depends on two hosts.** The 4.34 / 2024-12
    IDE the plugin targets is served only from `archive.eclipse.org` and
    `www.eclipse.org`. Both are now reachable from Cursor Cloud, so fetch the release
    the plugin actually targets. `archive.eclipse.org` has returned a single TLS reset
    on one run and then served the download on retry, so give it one retry before
    concluding it is blocked. If it really is unreachable, the **current** release from
    `download.eclipse.org` works too — the 4.34-built plugin runs fine on it. Install
    the built feature — the build produces `update-site/target/repository` — with the p2
    director, adding the lsp4e and release repositories alongside the local one:
    `<eclipse>/eclipse -nosplash -application org.eclipse.equinox.p2.director -repository file:<repo>/update-site/target/repository,https://download.eclipse.org/lsp4e/releases/<ver>,https://download.eclipse.org/releases/<rel> -installIU io.snyk.scanner.feature.group -destination <eclipse> -profile epp.package.java`.
  - **The Snyk view needs WebKitGTK.** It renders through an SWT `Browser` widget, so
    without the library the view dies with `SWTError: No more handles because there is
    no underlying browser available`, surfacing as `ClassCastException: ErrorViewPart
    cannot be cast to … SnykToolView` and "Failed to create the part's controls". Fix
    with `sudo apt-get install -y libwebkit2gtk-4.1-0` and relaunch. Note only one
    Eclipse may hold a `-data` workspace: kill any prior instance and remove
    `.metadata/.lock` first.
  - **Secure storage blocks language-server startup, and the real cause is an Equinox
    race.** `SnykLanguageServer.start()` waits on `isSecureStorageReady()`, which waits on
    the `Preferences` constructor's async `waitForSecureStorage()`. If that first encrypted
    write fails, the Snyk view sits at "Snyk Security is loading…" for ever with no error.
    On a headless Linux VM the failure is
    `NullPointerException: null algorithm name` from `SecretKeyFactory.getInstance`, reached
    through Equinox `JavaEncryption.internalEncrypt`: `JavaEncryption.init()` publishes its
    `initialized` flag before the cipher and key-factory fields are set, so a concurrent
    encryption path skips initialisation and uses null algorithm fields. Pre-seeding the
    keyring with cipher metadata avoids the first-write path and fixes it:

    ```properties
    org.eclipse.equinox.security.preferences.version=1
    org.eclipse.equinox.security.preferences.cipher=PBEWithHmacSHA512AndAES_256
    org.eclipse.equinox.security.preferences.keyFactory=PBEWithHmacSHA512AndAES_256
    ```

    Write that to the file `-eclipse.keyring` points at, **only when it is absent or
    empty** — it later holds encrypted credentials. Point `-eclipse.keyring` at that real
    file, not an empty stub, and pair it with `-eclipse.password`. Neither a
    gnome-keyring daemon nor `DBUS_SESSION_BUS_ADDRESS` is sufficient on its own, and
    neither is required once the keyring is seeded. You do **not** need to force
    `defaultPasswordProvider` either — leave it at its default. Guard the write with
    `[ ! -s <file> ]`, and in `eclipse.ini` remember the **first occurrence of each argument
    wins**, so de-duplicate rather than prepending again. Separately, dismissing or cancelling
    the password dialog leaves storage in a state that blocks startup permanently; reset it.
  - **The language server can hang before it ever scans, with no controlling terminal.**
    It shells out to `bash --login -i -c printenv` to load environment; with no TTY that
    child takes `SIGTTIN` and sits stopped (`T+`) indefinitely, so no scanner process
    starts behind it. The scan reports in progress for ever with `0 total` and no error.
    Set `SNYK_LS_DISABLE_SHELL_ENV_LOADING=1`. Note the knock-on: with shell loading off
    the LS inherits only the launching process's `PATH`, so a Maven project whose
    dependency extraction needs `mvn` fails with child exit `-2` and Open Source reports
    `scan failed` unless Maven is on that `PATH`.
- **The test suite overwrites the managed CLI.** Running the Tycho tests replaces the binary
  at `~/.snyk/snyk-linux` with a small test artifact, so the next real launch reports the LS
  protocol as `unknown` and fails to start. Delete that file and let managed binaries download
  it again. Tests arguably should not write to the shared managed-CLI path.
- **Authentication does not come from the environment.** The plugin passes the token
  held in its own preferences (Window > Preferences > Snyk) to the language server, so
  neither the ambient `SNYK_TOKEN` nor the CLI's `~/.config/configstore` authenticates
  it — `snyk auth` in a terminal has no effect. Use the API-token method rather than
  OAuth2, whose browser flow times out here — and note the **Re-authenticate** dialog
  defaults to OAuth and does not offer a token at all. The token path is Window >
  Preferences > Snyk > Setup > Authentication method > `API token (legacy)`. Trust the
  project too: the plugin's folder-trust gate is separate from Eclipse's own, and a scan
  will not run until it is satisfied.
- **A scan that sits at "in progress" and never completes.** Several distinct causes
  present identically, so read the Eclipse log before assuming a defect. Two are covered
  above — the secure-storage failure and the shell-environment hang. The third is an
  untrusted folder: the language server skips paths it does not trust and surfaces nothing
  in the view, while the CLI does the same project in seconds because it has no trust gate.
  The tell is `skipping scan of untrusted path path=…`. Trust the folder **through the
  plugin**: Window > Preferences > Snyk > Setup > **Trust settings > Trusted folder paths**,
  then *Add folder* — that section sits near the bottom and is easy to miss, you have to
  scroll the Preferences content and expand it, and it only renders once the language server
  is actually running — confirm that first with
  `pgrep -af 'snyk-(linux|macos|win) language-server'` (Windows without a POSIX shell:
  `tasklist | findstr snyk-win` or `Get-Process snyk-win`) rather than assuming from wall-clock
  time. Applying immediately triggers a scan. Writing `trustedFolders` into
  `~/.config/snyk/ls-config-Eclipse IDE` by hand does not work, even after a restart. If the
  log shows none of the three, capture it — that is a genuine bug report.
- **Do not hardcode a reachable/blocked host list.** The Cursor Cloud allowlist is
  stable — it changes when someone asks an admin to change it, not on its own — but that
  is exactly why a list written into a doc goes stale: it is still describing the world
  before the last request landed. Probe the hosts this build actually needs instead. Matching is per hostname, and a bare entry is apex-exact
  while `*.example.com` covers subdomains only, so an apex host has to be
  allowlisted in its own right (the update site `https://snyk.io/ide-plugins/` is
  one such apex). A block surfaces as a TLS reset — target-platform resolution
  reports it as an opaque "Connection reset" — rather than a DNS failure, so check
  a host directly:
  `timeout 12 openssl s_client -connect download.eclipse.org:443 -servername download.eclipse.org </dev/null`.
  This build needs `repo.maven.apache.org`, `download.eclipse.org` and
  `repo.eclipse.org`; the Language-Server download tests additionally fetch the
  Snyk CLI/LS binary from Snyk's CDN.
