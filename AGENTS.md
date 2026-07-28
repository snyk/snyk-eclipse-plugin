# AGENTS.md

Build/run/contributor basics live in [`README.md`](README.md) and
[`CONTRIBUTING.md`](CONTRIBUTING.md). This file adds Cursor Cloud specifics.

## Cursor Cloud specific instructions

Environment notes for Cursor Cloud agents. This is a Maven/Tycho Eclipse plugin.
The Maven wrapper (`./mvnw`) fetches Maven 3.9.4 from `repo.maven.apache.org`
automatically; there is no separate dependency-install step in the update script.

- **Use JDK 17, not JDK 21.** The target platform's execution environment is
  `JavaSE-17`. Building the plugin works under JDK 21, but the test module's
  Mockito/ByteBuddy inline mocks throw `Mockito cannot mock this class` under
  JDK 21 (every mock-based test errors). Set
  `export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64` before invoking Maven and
  the full `./mvnw clean verify` passes (400 tests, 0 failures).
- **Commands**: `./mvnw package` builds/packages the plugin (jar under
  `plugin/target/`); `./mvnw clean verify` also runs the headless Tycho tests.
- **Network**: the target platform pulls p2 repositories from
  `download.eclipse.org` (and Maven Central via `repo.maven.apache.org`). Both
  must be reachable; if egress is restricted, target-platform resolution fails
  with "Connection reset" against `download.eclipse.org`.
- The tests run headless via Tycho surefire (no display needed). Launching the
  plugin in a real Eclipse IDE (`runIde`-style) does need a GUI and is out of
  scope for headless cloud runs.
