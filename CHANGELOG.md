# Snyk Changelog

## [2.0.0] - Unreleased
### Changes

- add organization preference to specify organization to use for LS scans

### Fixes
- support download of Language Server for Apple M1
- fix download of Language Server in some situations 

### Changes
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
