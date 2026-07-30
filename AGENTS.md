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
  and passes 400 tests. Do **not** pass `-P sign` — it requires signing certs.
- **Add `-Declipse.p2.mirrors=false`.** Tycho otherwise follows p2 mirror redirects
  out to arbitrary university and ISP mirrors that cannot be enumerated in an
  egress allowlist; the flag forces direct downloads from `download.eclipse.org`.
- **Tests run headless** under Tycho surefire, so no display is needed. Launching
  the plugin inside a real Eclipse IDE does need a GUI and is out of scope for a
  headless cloud VM.
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
