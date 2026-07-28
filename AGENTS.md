# AGENTS.md

Snyk Eclipse plugin — a Maven/Tycho project (Eclipse plugin + feature + update
site) that embeds the Snyk CLI/Language Server. See [README.md](README.md),
[CONTRIBUTING.md](CONTRIBUTING.md) and `.cursorrules` for contributor guidance
and coding standards. This file adds Cursor Cloud environment notes.

## Cursor Cloud specific instructions

Durable, non-obvious notes for agents running in the Cursor Cloud Linux VM.

- **Build/test with JDK 17, not the default JDK 21.** The project targets Java 17
  (CI uses Temurin 17) and the test suite mocks with Mockito's inline mock-maker.
  Under the VM's default JDK 21 the tests fail with hundreds of
  `MockitoException: Could not modify all classes [class java.lang.Object, …]`
  (ByteBuddy self-attach). Compilation/packaging succeed on 21, but **tests need
  JDK 17**. Point `JAVA_HOME` at a JDK 17 before building:
  `export JAVA_HOME=/usr/lib/jvm/jdk-17.0.13+11; export PATH="$JAVA_HOME/bin:$PATH"`
  (install a Temurin 17 if one is not present, e.g. from the
  `adoptium/temurin17-binaries` GitHub release).
- **Standard command:** `./mvnw clean verify -DtrimStackTrace=false` (the CI
  command). The Maven wrapper fetches Maven from `repo.maven.apache.org`; the
  Tycho target platform pulls p2 repos from `download.eclipse.org` /
  `repo.eclipse.org`; and the LS-download tests fetch the Snyk CLI/LS binary from
  Snyk's CDN — all reachable in the VM. A clean run compiles all bundles and runs
  400 tests (0 failures/errors). Do **not** pass `-P sign` (needs signing certs).
- **Reachable:** `repo.maven.apache.org`, `download.eclipse.org`, `repo.eclipse.org`,
  `static.snyk.io`, `downloads.snyk.io`, `github.com`.
