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
  is actually running. Applying immediately triggers a scan. Writing `trustedFolders` into
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
