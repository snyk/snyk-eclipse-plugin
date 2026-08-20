#!/usr/bin/env bash
# scripts/setup.sh — one-shot local dev setup for snyk-eclipse-plugin.
#
# Verifies JDK, primes the Maven build, and prints the remaining manual
# Eclipse IDE steps that cannot be automated from the shell.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# Colours
red()    { printf '\033[31m%s\033[0m\n' "$*"; }
green()  { printf '\033[32m%s\033[0m\n' "$*"; }
yellow() { printf '\033[33m%s\033[0m\n' "$*"; }
bold()   { printf '\033[1m%s\033[0m\n' "$*"; }

REQUIRED_JAVA_MAJOR=17
SUPPORTED_JAVA_MAX=21   # CI uses 17; >21 is untested

bold "==> 1/4  Checking Java"

if ! command -v java >/dev/null 2>&1; then
    red "java not found on PATH."
    yellow "Install JDK $REQUIRED_JAVA_MAJOR (Temurin recommended). With sdkman:"
    echo "    curl -s 'https://get.sdkman.io' | bash"
    echo "    sdk install java ${REQUIRED_JAVA_MAJOR}.0.13-tem"
    exit 1
fi

JAVA_VERSION_RAW="$(java -version 2>&1 | head -1)"
JAVA_MAJOR="$(echo "$JAVA_VERSION_RAW" \
    | sed -E 's/.*"([0-9]+)\.[0-9]+.*/\1/; s/.*"1\.([0-9]+).*/\1/')"

echo "  Detected: $JAVA_VERSION_RAW"
echo "  JAVA_HOME=${JAVA_HOME:-<unset>}"

if [ "$JAVA_MAJOR" -lt "$REQUIRED_JAVA_MAJOR" ]; then
    red "Java $JAVA_MAJOR is too old. Need >= $REQUIRED_JAVA_MAJOR."
    exit 1
fi

if [ "$JAVA_MAJOR" -gt "$SUPPORTED_JAVA_MAX" ]; then
    yellow "Java $JAVA_MAJOR is newer than the CI-tested version ($REQUIRED_JAVA_MAJOR)."
    yellow "Tycho/PDE may behave differently. CI matrix uses Temurin 17."
fi

if [ -z "${JAVA_HOME:-}" ]; then
    yellow "JAVA_HOME is not set. Eclipse may pick a different JDK than mvnw."
fi

green "  Java OK"

bold "==> 2/4  Checking Maven wrapper"
./mvnw -version | sed 's/^/    /'
green "  Maven wrapper OK"

bold "==> 3/4  Building (mvnw package, skipping tests)"
echo "  This downloads the Eclipse target platform + all Maven deps."
echo "  First run: ~5-15 minutes. Cached run: <30 seconds."
./mvnw package -DskipTests -q

UPDATE_SITE_ZIP="$(ls update-site/target/update-site-*.zip 2>/dev/null | head -1 || true)"
if [ -z "$UPDATE_SITE_ZIP" ]; then
    red "Build finished but update-site zip not found."
    red "Inspect update-site/target/ and re-run with -X for diagnostics."
    exit 1
fi
green "  Built $UPDATE_SITE_ZIP"

bold "==> 4/4  Manual Eclipse IDE steps (cannot be scripted)"
cat <<'EOF'

  Now open Eclipse and:

  1. Eclipse: download "Eclipse IDE for RCP and RAP Developers"
     (this package already bundles PDE — no separate install needed).
     Apple Silicon: pick the aarch64 build.

  2. File > Import > Maven > Existing Maven Projects
     Root directory: this repo.
     Select the parent pom.xml and all submodules.

  3. Open target-platform/target-platform.target
     Click "Reload Target Platform" (top right).
     Wait until the target resolves (status bar at bottom).

  4. To launch a sandbox Eclipse with the plugin loaded:
     Open plugin/plugin.xml, go to the "Overview" tab,
     click "Launch an Eclipse application".

  If you hit "org.eclipse.* cannot be resolved" errors,
  re-do step 3. If external JARs are missing, the
  plugin/target/dependency/ directory now contains them
  (already populated by this script).

EOF

green "Setup complete."
