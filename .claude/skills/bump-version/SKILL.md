---
name: bump-version
description: Bump the snyk-eclipse-plugin release version across all files that carry it. Use when asked to bump/release the version, prepare a release, or when touching RELEASE.md. This skill may need updating if a Snyk Eclipse plugin version field is added or removed in a MANIFEST.MF, pom.xml, or category.xml.
---

## Before touching any file

1. Read the current version from `pom.xml` (`<version>X.Y.Z-SNAPSHOT</version>`).
2. Check whether a bump is even due, without needing a target version up front:
   - `git fetch --tags origin`
   - Find the latest release tag: `git describe --tags --abbrev=0 origin/main` (or `gh release list --limit 1`).
   - Read that tag's `pom.xml` version: `git show <tag>:pom.xml | grep -m1 '<version>'`.
   - Compare it to your local `pom.xml` version (`grep -m1 '<version>' pom.xml`).
   - Same version on both → nothing has moved past the last release yet → a bump is due, proceed.
   - Local already ahead of the tagged version → someone already bumped → stop and tell the user.

## The version lives in 11 files, in two conventions

Maven `pom.xml` files use `X.Y.Z-SNAPSHOT`:
- `pom.xml`, `feature/pom.xml`, `plugin/pom.xml`, `target-platform/pom.xml`, `tests/pom.xml`, `update-site/pom.xml`

OSGi/Eclipse files use `X.Y.Z.qualifier`:
- `plugin/META-INF/MANIFEST.MF`, `tests/META-INF/MANIFEST.MF` (`Bundle-Version:` line)
- `feature/feature.xml` (`version=` attribute)
- `feature/category.xml`, `update-site/category.xml` (both the `version=` attribute and the jar filename in the `url=` attribute)

Update both occurrences in `feature/category.xml` and `update-site/category.xml`. Don't touch `feature/feature.xml`'s `<plugin ... version="0.0.0">` entry — `0.0.0` is Tycho's auto-resolve placeholder, not a version literal.

## Check whether the LS protocol version also needs bumping

The plugin pins a required Language Server protocol version in `plugin/src/main/java/io/snyk/languageserver/download/LsBinaries.java` (`REQUIRED_LS_PROTOCOL_VERSION`). Compare it against snyk-ls's current release value:

```
git -C <path-to-snyk-ls-checkout> fetch origin main
git -C <path-to-snyk-ls-checkout> show origin/main:.goreleaser.yaml | grep LS_PROTOCOL_VERSION
```

If they match, no action needed. If the plugin's pinned version is behind, flag it to the user and ask whether to bump `REQUIRED_LS_PROTOCOL_VERSION` as part of this release or leave it.

## Decide the target version

Every historical bump in this repo increments the minor digit and leaves patch at 0 (`X.Y.0` → `X.(Y+1).0`) — there is no precedent for a true patch-digit bump (`X.Y.Z` → `X.Y.(Z+1)`). If asked for a "patch" bump specifically, flag the mismatch with what history shows and ask which the user actually wants rather than assuming. They may actually want an emergency hotfix, but this isn't something that has been done before.

## Make the change, then verify against precedent

After editing, cross-check against 2-4 prior bump commits. Find them from `pom.xml`'s own history rather than trusting commit message wording (not every bump commit is worded the same way):

- `git log --oneline -- pom.xml` — pick a few recent candidates.
- `git show --stat <sha>` on each — a real version bump touches all 11 files listed above with one line changed each; skip anything else (dependency bumps, LS protocol bumps, changelog edits also touch `pom.xml` but look different).

Confirm your own change matches that shape:
- Same 11 files touched, nothing more, nothing fewer. Unless you did a LS protocol version bump as well.
- `git diff --stat` shows `11 files changed, 11 insertions(+), 11 deletions(-)`. Unless you did a LS protocol version bump as well.

If the file set or line count doesn't match, figure out why.

## Commit

Follow this repo's standard commit workflow — see `AGENTS.md`.
