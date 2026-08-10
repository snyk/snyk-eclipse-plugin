# AGENTS.md

The Snyk Eclipse plugin is a Maven/Tycho project (plugin, feature and update site)
that embeds the Snyk CLI and Language Server. Build, run and contributor guidance
lives in [README.md](README.md), [CONTRIBUTING.md](CONTRIBUTING.md) and
`.cursorrules` — read those first. This file adds only Cursor Cloud environment
notes.

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
  - **A pre-baked workspace traps you in a Secure Storage prompt loop.** On a saved
    cloud environment `~/eclipse-workspace` already exists, and launching against it
    produces repeated "Enter Password to Unlock the Secure Storage" dialogs, because
    the plugin reads stored credentials while Eclipse's master password was never
    set. It looks like a plugin fault and it blocks unattended runs completely.
    Interactively, dismiss or set a password once. For an unattended run, use a fresh
    `-data` workspace or clear `~/.eclipse/org.eclipse.equinox.security` before
    launching.
- **Authentication does not come from the environment.** The plugin passes the token
  held in its own preferences (Window > Preferences > Snyk) to the language server, so
  neither the ambient `SNYK_TOKEN` nor the CLI's `~/.config/configstore` authenticates
  it — `snyk auth` in a terminal has no effect. Use the API-token method rather than
  OAuth2, whose browser flow times out here, and trust the project in the Snyk UI: the
  plugin's folder-trust gate is separate from Eclipse's own, and a scan will not run
  until it is satisfied.
- **A scan stuck at "in progress" for ever is almost always an untrusted folder, not a
  bug.** The language server **silently skips paths it does not trust** and surfaces
  nothing in the view, so it looks like a hung scan — while the CLI scans the same
  project in seconds, because it has no trust gate. The tell is in the Eclipse log:
  `skipping scan of untrusted path path=…`. Three cloud runs read this as a plugin or
  LS defect before the log line was spotted. Trust it **through the plugin**: use the
  *Trust folder* affordance the Snyk view offers, or the trusted-folders list in Snyk
  preferences on the **Setup** tab — which sits near the bottom and may be invisible
  until the Preferences window is enlarged or scrolled. Writing `trustedFolders` into
  `~/.config/snyk/ls-config-Eclipse IDE` by hand does **not** work, even after a
  restart.
- **Probe egress instead of trusting a host list.** The allowlist changes between
  runs, so treat any reachable/blocked list — including in older revisions of this
  section — as stale. Matching is per hostname, and a bare entry is apex-exact
  while `*.example.com` covers subdomains only, so an apex host has to be
  allowlisted in its own right (the update site `https://snyk.io/ide-plugins/` is
  one such apex). A block surfaces as a TLS reset — target-platform resolution
  reports it as an opaque "Connection reset" — rather than a DNS failure, so check
  a host directly:
  `timeout 12 openssl s_client -connect download.eclipse.org:443 -servername download.eclipse.org </dev/null`.
  This build needs `repo.maven.apache.org`, `download.eclipse.org` and
  `repo.eclipse.org`; the Language-Server download tests additionally fetch the
  Snyk CLI/LS binary from Snyk's CDN.
