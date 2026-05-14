# IDE-1652 Implementation Plan

Branch: `feat/IDE-1652`
Reference: https://github.com/snyk/vscode-extension/pull/733

**IMPORTANT: Do NOT push to remote. Do NOT run `git push`. Local commits only.**

---

## Status

- [x] Phase 1 — SettingsRegistry (commit 7d8c693)
- [x] Phase 2 — CLI protocol version gate (commit f2387c0)
- [x] Phase 3 — `addAll()` wipe safety (commit 77589bd)

---

## Context

Eclipse plugin implements `$/snyk.configuration` listener + `workspace/didChangeConfiguration` outbound for LS protocol v25+. VSCode PR #733 is the reference implementation.

Key files:
- `plugin/src/main/java/io/snyk/languageserver/LsSettingsKeys.java` — LS key constants
- `plugin/src/main/java/io/snyk/languageserver/LsFolderSettingsKeys.java` — folder-scoped key constants
- `plugin/src/main/java/io/snyk/languageserver/LsConfigurationUpdater.java` — builds + sends outbound `LspConfigurationParam`
- `plugin/src/main/java/io/snyk/languageserver/protocolextension/SnykExtendedLanguageClient.java` — receives `$/snyk.configuration`, calls `persistGlobalSettings()`
- `plugin/src/main/java/io/snyk/languageserver/protocolextension/messageObjects/ConfigSetting.java` — `{ value, changed, source, originScope, isLocked }`
- `plugin/src/main/java/io/snyk/languageserver/protocolextension/messageObjects/LspConfigurationParam.java` — top-level payload
- `plugin/src/main/java/io/snyk/languageserver/protocolextension/messageObjects/LspFolderConfig.java` — per-folder payload
- `plugin/src/main/java/io/snyk/eclipse/plugin/properties/FolderConfigSettings.java` — in-memory folder config store
- `plugin/src/main/java/io/snyk/languageserver/SnykLanguageServer.java` — LS process start, `getInitializationOptions()`
- `plugin/src/main/java/io/snyk/eclipse/plugin/preferences/Preferences.java` — pref store, `isExplicitlyChanged()`, `storeAndTrackChange()`

Dropped from scope (intentional):
- `isLocked` UI enforcement — LS HTML handles its own lock rendering; not done in VSCode either
- `hover_verbosity` — never existed in Eclipse; VSCode-only addition
- Feedback loop suppression — `prefs.store()` does NOT trigger `configurationChanged()`; non-issue confirmed

---

## Phase 1 — SettingsRegistry

**Goal:** Replace two hardcoded maps with a single registry. Make outbound build + inbound persist use the same source of truth.

**Problem today:**
1. `LsConfigurationUpdater.java` has a big inline block building settings one-by-one with `addMachineSetting()` 
2. `SnykExtendedLanguageClient.java:433` has an 8-key `LS_TO_PREF_KEY` map for inbound persistence — drops all other keys silently
3. `snyk_secrets_enabled` is missing entirely from both outbound and inbound

**What to build:**

Create `plugin/src/main/java/io/snyk/languageserver/LsSettingsRegistry.java`:

```java
package io.snyk.languageserver;

// A registry entry binding an LS key to an Eclipse pref key with defaults and converters.
// Used for both outbound (building LspConfigurationParam) and inbound (persisting $/snyk.configuration).
public final class LsSettingsRegistry { ... }
```

Each entry needs:
- `lsKey` (String) — e.g. `"snyk_code_enabled"`
- `prefKey` (String) — e.g. `Preferences.ACTIVATE_SNYK_CODE_SECURITY`
- `defaultValue` (String) — used for outbound if pref not set
- `outboundSerializer` (Function<String, Object>) — converts pref string → LS value (e.g. `"true"` → `Boolean.TRUE`)
- `inboundDeserializer` (Function<Object, String>) — converts LS value → pref string (for `persistGlobalSettings`)
- `alwaysChanged` (boolean) — true for entries like `automatic_authentication=false`, `trust_enabled=true`

VSCode's `SETTINGS_REGISTRY` global keys to cover (align with `serverSettingsToLspConfigurationParam.ts`):

| LS key | Eclipse pref key | Notes |
|--------|-----------------|-------|
| `api_endpoint` | `Preferences.ENDPOINT_KEY` | |
| `token` | `Preferences.AUTH_TOKEN_KEY` | |
| `organization` | `Preferences.ORGANIZATION_KEY` | |
| `authentication_method` | `Preferences.AUTHENTICATION_METHOD` | |
| `automatic_authentication` | — | always `false`, alwaysChanged=true |
| `snyk_code_enabled` | `Preferences.ACTIVATE_SNYK_CODE_SECURITY` | bool |
| `snyk_oss_enabled` | `Preferences.ACTIVATE_SNYK_OPEN_SOURCE` | bool |
| `snyk_iac_enabled` | `Preferences.ACTIVATE_SNYK_IAC` | bool |
| `snyk_secrets_enabled` | `Preferences.ACTIVATE_SNYK_SECRETS` (add if missing) | bool — NEW |
| `scan_automatic` | `Preferences.SCANNING_MODE_AUTOMATIC` | `"automatic"`↔bool |
| `scan_net_new` | `Preferences.ENABLE_DELTA` | bool |
| `send_error_reports` | `Preferences.SEND_ERROR_REPORTS` | |
| `automatic_download` | `Preferences.MANAGE_BINARIES_AUTOMATICALLY` | bool |
| `cli_path` | `Preferences.CLI_PATH` | |
| `binary_base_url` | `Preferences.CLI_BASE_URL` | default `https://downloads.snyk.io` |
| `proxy_insecure` | `Preferences.INSECURE_KEY` | bool |
| `additional_parameters` | `Preferences.ADDITIONAL_PARAMETERS` | |
| `additional_environment` | `Preferences.ADDITIONAL_ENVIRONMENT` | |
| `trust_enabled` | — | always `true`, alwaysChanged=false |
| `risk_score_threshold` | `Preferences.RISK_SCORE_THRESHOLD` | int, null if 0 |
| `enabled_severities` | `Preferences.FILTER_SHOW_*` (4 keys) | composite — keep existing logic, just move it |
| `issue_view_open_issues` | `Preferences.FILTER_IGNORES_SHOW_OPEN_ISSUES` | bool |
| `issue_view_ignored_issues` | `Preferences.FILTER_IGNORES_SHOW_IGNORED_ISSUES` | bool |
| `cli_release_channel` | `Preferences.RELEASE_CHANNEL` | |
| `enable_telemetry` | `Preferences.ENABLE_TELEMETRY` | |

Note: `enabled_severities` and `risk_score_threshold` have special serialization — keep the existing inline logic, just move it into the registry entry's serializer lambda.

**After registry exists:**
- `LsConfigurationUpdater.buildConfigurationParam()` iterates registry to build `settings` map (removing the big inline block). Non-registry fields (`trusted_folders`, metadata) stay inline.
- `SnykExtendedLanguageClient.persistGlobalSettings()` iterates registry for inbound — replaces the 8-key map.

**Tests:** Update `LsConfigurationUpdaterTest` + `SnykExtendedLanguageClientTest` to cover newly-persisted keys.

Check if `Preferences.ACTIVATE_SNYK_SECRETS` exists before adding it. Grep `plugin/src/main/java/io/snyk/eclipse/plugin/preferences/Preferences.java` for `SECRETS`.

---

## Phase 2 — CLI protocol version gate

**Goal:** Match VSCode behavior — abort LS start if CLI protocol version doesn't match expected.

**Where:** `SnykLanguageServer.start()` in `plugin/src/main/java/io/snyk/languageserver/SnykLanguageServer.java`

**Expected protocol version:** `LsBinaries.REQUIRED_LS_PROTOCOL_VERSION` (already used in `LsConfigurationUpdater`). Check its type — it's a `String` in `LspConfigurationParam.setRequiredProtocolVersion()`. The CLI returns an integer. Parse + compare as int.

**Implementation:**
```java
// In SnykLanguageServer.start(), after getCliPathOrThrow(), before setCommands():
verifyCliProtocolVersion(cliPath);
```

```java
static void verifyCliProtocolVersion(String cliPath) throws IOException {
    try {
        ProcessBuilder pb = new ProcessBuilder(cliPath, "language-server", "--protocolVersion");
        pb.redirectErrorStream(true);
        Process proc = pb.start();
        String output = new String(proc.getInputStream().readAllBytes()).trim();
        proc.waitFor(5, TimeUnit.SECONDS);
        int actual = Integer.parseInt(output);
        int expected = Integer.parseInt(LsBinaries.REQUIRED_LS_PROTOCOL_VERSION);
        if (actual != expected) {
            String msg = "Snyk CLI protocol version mismatch: expected " + expected + ", got " + actual
                + ". Please update the Snyk CLI.";
            SnykLogger.logAndShow(msg);
            throw new IOException(msg);
        }
    } catch (NumberFormatException | InterruptedException e) {
        SnykLogger.logError(e);
        // Non-fatal: if we can't parse, let startup proceed
    }
}
```

Check actual type/value of `LsBinaries.REQUIRED_LS_PROTOCOL_VERSION` before implementing — read `plugin/src/main/java/io/snyk/languageserver/download/LsBinaries.java`.

**Tests:** Add to `SnykLanguageServerTest` — mock CLI returning correct version (passes), wrong version (throws IOException), unparseable output (passes with log).

---

## Phase 3 — `addAll()` wipe safety

**Goal:** Ensure inbound `$/snyk.configuration` doesn't wipe user-pending folder overrides.

**Investigate first:**
1. Read `FolderConfigSettings.addAll()` — it calls `configs.clear()` then re-adds. This wipes everything.
2. Determine: can a user make a folder change (e.g. set base branch in `NativeProjectPropertyPage`) while waiting for `$/snyk.configuration` response? If yes, that change is lost.
3. Check: does `$/snyk.configuration` arrive *after* the IDE sends `didChangeConfiguration`? Or can it arrive at any time (e.g. tenant admin changes remote config)?

**Likely fix:** Merge strategy — for each inbound folder config, only overwrite keys where `changed != true` in the existing in-memory config. Keys the user has set with `changed=true` survive the merge.

```java
// Instead of configs.clear() + re-add:
public synchronized void mergeAll(List<LspFolderConfig> inbound) {
    for (LspFolderConfig incoming : inbound) {
        String key = normalizePath(incoming.getFolderPath());
        LspFolderConfig existing = configs.get(key);
        if (existing == null) {
            configs.put(key, incoming);
        } else {
            configs.put(key, mergeConfigs(existing, incoming));
        }
    }
}

private LspFolderConfig mergeConfigs(LspFolderConfig existing, LspFolderConfig incoming) {
    // incoming wins for all keys EXCEPT keys where existing has changed=true
    // (user override in flight)
}
```

Decide after investigation whether `addAll()` replace-all is actually safe (e.g. if LS always echoes back what we sent, then replace-all is fine).

---

## Build & verify

After each phase:
```bash
./mvnw clean verify -pl plugin,tests
# Check PMD:
./mvnw pmd:check -pl plugin
# Zero violations required — check plugin/target/pmd.xml
```

**Do NOT run `git push` at any point.**
