# Snyk Changelog

## [2.0.0] - Unreleased
### Changes
- Increase LS version

## [2.0.0] - v20230131.163308
### Changes
- Snyk Test now scans the selected project
- Added new Snyk Test Workspace command

## [2.0.0] - v20221222.164444
### Changes
- Submit runtime information of jdk and os to language server

### Fixed
- NPE when finishing Snyk Wizard in empty workspace

## [2.0.0] - v20221220.094322
### Changes
- fix NPEs in Snyk Wizard
- update jackson deps to 2.14.0

## [2.0.0] - v20221130.134443
### Changes
- add folder trust feature

## [2.0.0] - v20221115.132308
### Changes
- adds configuration wizard for custom endpoints

## [2.0.0] - v20221007.135736
### Fixed
- malformed proxy URL

## [2.0.0] - v20220927.182222
### Changes
- add support for insecure and custom CAs to download and API checks

## [2.0.0] - v20220905.164345
### Changes
- promote language server from BETA to GA
- announce workspace folder capability correctly
- disable / enable Snyk Code based on org settings

## [2.0.0] - v20220818.075149
### Changes
- add support for `window/showDocument` request
- automatically start workspace scan on plugin startup
- allow manually triggering a workspace scan via context menu or Snyk View

### Fixed
- Language Server does not shutdown anymore on disconnecting last document, it now uses a grace period of one hour

## [2.0.0] - v20220725.070608
### Changes
- refactor LSP extensions to use snyk namespace - this will force an LS Server update
- configure LS with initialize options instead of env vars
- remove redundant CLI download
- allow automatic/manual Snyk binary management
- make CLI path configurable
- pre-fill settings values from SNYK_API, SNYK_TOKEN, SNYK_CFG_ORG environment variables on first time plugin usage

## [2.0.0] - v20220718.111138
### Changes
- enable telemetry by default

## [2.0.0] - v20220627.112145

### Fixes
- parse and display findings of multi-module projects in Snyk View, supporting the `--all-projects` parameter there as well.
- use preferences to configure additional environment variables in environment of CLI
- proxy configuration for https proxy is now using `https_proxy=http://configured-proxy-settings-in-eclipse` instead of `https_proxy=https://configured-proxy-settings-in-eclipse`
- support additional environment variables of the format `a=b=c`, e.g. needed for `MAVEN_OPTS=-Djava.awt.headless=true`


## [2.0.0] - v20220620.201253

### Fixes
- fixed ConcurrentModificationException when submitting configuration to language server
- don't shutdown Language Server when all associated files are closed, in order to preserve cached diagnostics for an hour

### Changes
- mark retrieved diagnostics as `Snyk` instead of `Language Server` to be able to filter, group and sort in problem view.

## [2.0.0] - v20220610.102110

### Fixes
- fixed legacy Snyk View scan under Windows
- fixed passing of --insecure and --org parameters if empty

### Changes
- ⬆️ update jackson dependencies to 2.13.3

## [2.0.0] - v20220525.165232

### Fixes
- support download of Language Server for Apple M1
- fix download of Language Server in some situations

### Changes
- add organization preference to specify organization to use for LS scans
- add preference for usage statistics (metrics)

## [2.0.0] - v20220517.090738

### Changes

- add setting to configure error reporting to Snyk
- enable hovers in JDT
- optimize dependencies & compatibility (2021-3 is the minimum Eclipse release now)
- add supported file types as content types and associate them with GenericEditor
- automatic weekly deployment with signed jars to update site / marketplace
- automatic CI/CD deployment on push to main with signed jars

### Fixes
- auto-enable legacy scan from Snyk View when token is retrieved via language server


## [2.0.0] - v20220510.101331

- fix missing progress bars for scan jobs
- Language Server: Support Snyk Open Source (activated by default)
- Language Server: Support Snyk Code (deactivated by default)
- Language Server: Support Snyk Infrastructure-as-Code (activated by default)
- Fully automated maven tycho build
- Provide an update site through CI/CD and publish it on github

## [2.0.0] - v20220509.163447

### Changes
- Integrated Language Server into Eclipse Plugin
- Automatic Download of Language Server
- Manual selection of path to Language Server
- Language Server: Support Snyk Open Source (activated by default)
- Language Server: Support Snyk Code (deactivated by default)
- Language Server: Support Snyk Infrastructure-as-Code (activated by default)
- Language Server: Support Eclipse Proxy Settings
- Language Server: Extended Configuration Options
- Secure Preference Storage for Token security

### Fixes

- Fixes UI bugs when Token not available

## [1.3.x]

Old version on marketplace
