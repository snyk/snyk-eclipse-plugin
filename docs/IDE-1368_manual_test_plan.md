# Manual Test Plan for IDE-1368: Get Project Organization from Language Server (Eclipse)

## Overview of Changes

**New Feature**: Organization selection in project property page
- **UI Enhancement**: Auto-select organization checkbox in project property page
- **Language Server Integration**: Organization settings are synchronized with the Snyk Language Server via FolderConfig
- **Fallback Logic**: Global organization setting serves as fallback when project-specific values are empty

## Test Cases Overview

This test plan includes the following test cases:

1. **Test Case 1: Organization Selection in Project Property Page**
   - Verify organization can be set per-project through the project property page

2. **Test Case 2: Auto-Select Organization Checkbox**
   - Verify auto-select organization functionality and UI behavior

3. **Test Case 3: Project Property Page UI Integration**
   - Verify complete project property page integration and field interactions

4. **Test Case 4: Language Server Configuration Updates**
   - Verify configuration changes trigger Language Server updates and are properly synchronized

5. **Test Case 5: Edge Cases and Error Handling**
   - Verify edge cases and error scenarios are handled gracefully

6. **Test Case 6: Integration with Scans**
   - Verify organization settings are correctly used during Snyk scans

7. **Test Case 7: Migration Testing**
   - Verify the extension correctly handles migrated folder configs received from Language Server

8. **Test Case 8: Global Organization Settings**
   - Verify global organization setting works correctly and serves as fallback

9. **Test Case 9: Opting In/Out of Automatic Org Selection** (Critical)
   - Verify opting in and out of automatic organization selection works correctly

10. **Test Case 10: Eclipse Restart Scenarios**
    - Verify organization settings persist correctly after Eclipse restart and folder configs are refreshed

---

## Key Concepts

### Organization Hierarchy (Fallback Order)
1. **Project-specific values** (highest priority)
   - `autoDeterminedOrg` when auto-detect is enabled (`orgSetByUser = false`)
   - `preferredOrg` when manual mode is enabled (`orgSetByUser = true`)
2. **Global organization setting** (fallback)
   - From `Preferences.ORGANIZATION_KEY` in Preferences Page
3. **Empty string** (final fallback)

### FolderConfig Fields
- `autoDeterminedOrg`: Organization automatically determined by Language Server from LDX-Sync
- `preferredOrg`: Organization manually set by user
- `orgSetByUser`: Boolean flag indicating if user has manually set organization (`true` = manual, `false` = auto-detect)
- `orgMigratedFromGlobalConfig`: Migration flag

### UI Behavior
- **Auto-detect checkbox checked** (`orgSetByUser = false`):
  - `projectOrgEditor` shows `autoDeterminedOrg` (read-only display, field is disabled)
  - `orgEditor` (global) is disabled
  - Language Server uses `autoDeterminedOrg` for scans
  
- **Auto-detect checkbox unchecked** (`orgSetByUser = true`):
  - `projectOrgEditor` is empty and editable for user input
  - `orgEditor` (global) is enabled
  - Language Server uses `preferredOrg` (or global org as fallback) for scans

### Special Case Handling
- **If `orgSetByUser = true` and `preferredOrg = ""`** when opening project property page:
  - Set `orgSetByUser = false` and use `autoDeterminedOrg` in `projectOrgEditor`
  - Update folder config and submit to Language Server

---

## Test Case 1: Organization Selection in Project Property Page

### Objective
Verify organization can be set per-project through the project property page.

### Prerequisites
- Eclipse IDE with Snyk plugin installed
- Authenticated Snyk account with multiple organizations
- A test project open in Eclipse
- Language Server initialized and folder configs received

### Steps

#### 1.1 Access Project Property Page
1. Right-click on a project in Project Explorer
2. Select **Properties**
3. Navigate to **Snyk** in the left sidebar
4. Verify **"Organization Settings"** section is visible
5. Verify **"Auto-select organization"** checkbox is present
6. Verify **"Organization"** text field is present

#### 1.2 Test Organization Field Visibility
1. Verify **"Organization"** field is visible in Organization Settings section
2. Verify help text/links are displayed
3. Click help links and verify documentation opens correctly

#### 1.3 Test Manual Organization Setting
1. Uncheck **"Auto-select organization"** checkbox
2. Verify **"Organization"** text field becomes enabled
3. Enter a valid organization ID in the **"Organization"** field
4. Verify **"Organization"** field in Preferences (global) becomes enabled
5. Click **"Apply"** or **"OK"**
6. Verify the setting is saved

#### 1.4 Test Organization Persistence
1. Close and reopen Project Properties dialog
2. Navigate to **Snyk**
3. Verify the organization setting is retained
4. Verify checkbox state matches the saved `orgSetByUser` value
5. Verify text field shows correct organization value

#### 1.5 Verify Language Server Communication
1. Open Eclipse Error Log (Window → Show View → Error Log)
2. Monitor for `WorkspaceDidChangeConfiguration` calls
3. Verify folder config is sent to Language Server with correct `preferredOrg` and `orgSetByUser` values

### Expected Results
- Organization field is visible and functional in project property page
- Settings are saved to FolderConfig
- Organization is used in subsequent scans
- Language Server receives updated configuration via `WorkspaceDidChangeConfiguration`

---

## Test Case 2: Auto-Select Organization Checkbox

### Objective
Verify auto-select organization functionality and UI behavior.

### Prerequisites
- Same as Test Case 1
- Language Server has received folder configs with `autoDeterminedOrg` populated

### Steps

#### 2.1 Test Checkbox Visibility and Initial State
1. Open **Project Properties → Snyk**
2. Verify **"Auto-select organization"** checkbox is present in Organization Settings section
3. Verify help text/links are displayed
4. Click help links and verify documentation opens correctly
5. Verify checkbox initial state matches `orgSetByUser` value from Language Server:
   - **Checked** if `orgSetByUser = false` (auto-detect enabled)
   - **Unchecked** if `orgSetByUser = true` (manual mode)

#### 2.2 Test Checkbox Behavior - Enabling Auto-Detect
1. If checkbox is unchecked, check **"Auto-select organization"**
2. Verify **"Organization"** field shows `autoDeterminedOrg` value (read-only display)
3. Verify **"Organization"** field is disabled
4. Verify **"Organization"** field in Preferences (global) becomes disabled
5. Verify checkbox tooltip explains auto-detect behavior

#### 2.3 Test Checkbox Behavior - Disabling Auto-Detect
1. Uncheck **"Auto-select organization"** checkbox
2. Verify **"Organization"** field becomes enabled
3. Verify **"Organization"** field is cleared (empty)
4. Verify **"Organization"** field in Preferences (global) becomes enabled
5. Verify user can now enter manual organization value

#### 2.4 Test Auto-Detect Functionality
1. Ensure checkbox is checked (auto-detect enabled)
2. Click **"Apply"**
3. Run a Snyk scan (manually or wait for auto-scan)
4. Verify scan uses `autoDeterminedOrg` from Language Server
5. Verify scan results are associated with the correct organization

#### 2.5 Test Manual Override
1. Uncheck **"Auto-select organization"**
2. Enter a specific organization ID in **"Organization"** field
3. Click **"Apply"**
4. Run a Snyk scan
5. Verify scan uses the manually entered organization
6. Verify `orgSetByUser = true` in folder config sent to Language Server

### Expected Results
- Checkbox controls organization field state correctly
- Auto-detect selects appropriate organization from Language Server
- Manual organization setting works when auto-detect is disabled
- Language Server receives correct `orgSetByUser` flag

---

## Test Case 3: Project Property Page UI Integration

### Objective
Verify complete project property page integration and field interactions.

### Prerequisites
- Eclipse IDE with Snyk plugin
- Test project open
- Language Server initialized

### Steps

#### 3.1 Test Property Page Loading
1. Open **Project Properties → Snyk**
2. Verify all sections load correctly:
   - Additional Parameters
   - Organization Settings
3. Verify no errors in Error Log

#### 3.2 Test Field Population on Dialog Open
1. With auto-detect **enabled** (`orgSetByUser = false`):
   - Verify **"Organization"** shows `autoDeterminedOrg` value
   - Verify checkbox is checked
   - Verify **"Organization"** field is disabled
   - Verify **"Organization"** (global) field in Preferences is disabled

2. With auto-detect **disabled** (`orgSetByUser = true`):
   - Verify **"Organization"** shows `preferredOrg` value (or empty if not set)
   - Verify checkbox is unchecked
   - Verify **"Organization"** field is enabled
   - Verify **"Organization"** (global) field in Preferences is enabled

3. With **no folder config** (Language Server not initialized):
   - Verify fields are disabled
   - Verify fields show appropriate default/empty values

#### 3.3 Test Field Validation
1. Enter invalid organization ID format in **"Organization"**
2. Test with empty fields
3. Test with very long organization strings
4. Verify **"Apply"** button behavior with invalid input

#### 3.4 Test Save/Cancel Operations
1. Make changes to organization settings:
   - Toggle auto-detect checkbox
   - Enter organization value
2. Click **"Apply"**
   - Verify changes are saved
   - Verify Language Server receives update
   - Verify dialog remains open
3. Make additional changes
4. Click **"Cancel"**
   - Verify changes are discarded
   - Verify original values are restored
5. Click **"OK"**
   - Verify changes are saved
   - Verify dialog closes

#### 3.5 Test Field Interactions
1. Verify **"Organization"** field behavior:
   - When checkbox is checked, field is disabled and shows `autoDeterminedOrg`
   - When checkbox is unchecked, field is enabled and cleared
2. Verify **"Organization"** (global) field in Preferences:
   - Enabled/disabled state matches auto-detect checkbox
   - Value persists independently of project settings

#### 3.6 Test Special Case: Empty PreferredOrg
1. Set `orgSetByUser = true` and `preferredOrg = ""` (via folder config or reset)
2. Open Project Properties → Snyk
3. Verify:
   - `orgSetByUser` is automatically set to `false`
   - `projectOrgEditor` shows `autoDeterminedOrg`
   - Checkbox is checked
   - Folder config is updated and sent to Language Server

### Expected Results
- Project property page loads without errors
- Fields populate correctly based on folder config state
- Save/cancel operations function properly
- Field interactions work as expected
- Special case handling works correctly

---

## Test Case 4: Language Server Configuration Updates

### Objective
Verify configuration changes trigger Language Server updates and are properly synchronized.

### Prerequisites
- Eclipse IDE with Snyk plugin
- Language Server running and initialized
- Multiple test projects (optional, for multi-project testing)

### Steps

#### 4.1 Test Configuration Propagation
1. Open **Project Properties → Snyk**
2. Change organization settings:
   - Toggle auto-detect checkbox
   - Enter/change preferred organization
3. Click **"Apply"**
4. Monitor Language Server communication:
   - Check Error Log for `WorkspaceDidChangeConfiguration` calls
   - Verify folder config contains updated values:
     - `preferredOrg` matches text field value
     - `orgSetByUser` matches checkbox state
     - `autoDeterminedOrg` is preserved (never modified by IDE)

#### 4.2 Test FolderConfig Update Logic
1. With auto-detect **enabled** (checkbox checked):
   - Click **"Apply"**
   - Verify folder config sent to LS has:
     - `orgSetByUser = false`
     - `preferredOrg = ""` (empty string)
     - `autoDeterminedOrg` unchanged (from LS)

2. With auto-detect **disabled** (checkbox unchecked):
   - Enter organization in **"Organization"** field
   - Click **"Apply"**
   - Verify folder config sent to LS has:
     - `orgSetByUser = true`
     - `preferredOrg` = value from text field
     - `autoDeterminedOrg` unchanged (from LS)

#### 4.3 Test Multiple Projects
1. Open multiple projects in Eclipse
2. Configure different organizations for different projects:
   - Project A: Auto-detect enabled
   - Project B: Manual organization "org-123"
   - Project C: Manual organization "org-456"
3. Switch between projects (open properties for each)
4. Verify each project maintains its own organization setting
5. Run scans on each project
6. Verify each project uses the correct organization

#### 4.4 Test Language Server Response
1. Make organization setting changes
2. Click **"Apply"**
3. Wait for Language Server to process configuration
4. Verify Language Server sends updated folder configs back via `$/snyk.folderConfig` notification
5. Verify IDE updates UI to reflect any changes from Language Server
6. Verify `autoDeterminedOrg` is updated by Language Server (if LDX-Sync provides new value)

#### 4.5 Test Fallback Behavior
1. Clear preferred organization (set to empty)
2. Disable auto-detect
3. Verify global organization setting is used as fallback
4. Verify Language Server receives global organization in `organization` field of `LanguageServerSettings`
5. Run scan and verify correct organization is used

### Expected Results
- Configuration changes are propagated to Language Server via `WorkspaceDidChangeConfiguration`
- Folder config updates contain correct `preferredOrg`, `orgSetByUser`, and `autoDeterminedOrg` values
- Each project maintains its own organization setting
- Language Server responses are properly handled
- Fallback to global organization works correctly

---

## Test Case 5: Edge Cases and Error Handling

### Objective
Verify edge cases and error scenarios are handled gracefully.

### Prerequisites
- Eclipse IDE with Snyk plugin
- Test project
- Various test scenarios (see steps)

### Steps

#### 5.1 Test Empty/Null Values
1. Test with `autoDeterminedOrg` empty/null:
   - Verify UI handles gracefully
   - Verify fallback to global organization works
   - Verify no crashes or errors

2. Test with `preferredOrg` empty when `orgSetByUser = true`:
   - Verify fallback behavior
   - Verify Language Server receives correct values

#### 5.2 Test Language Server Not Initialized
1. Close project
2. Open Project Properties (project must be open to access properties)
3. Verify fields are disabled or show appropriate message
4. Verify no errors when accessing properties without Language Server

#### 5.3 Test Rapid Toggle
1. Rapidly toggle auto-detect checkbox multiple times
2. Enter/clear organization value multiple times
3. Click Apply/Cancel rapidly
4. Verify no race conditions or UI glitches
5. Verify final state is correct

#### 5.4 Test Special Characters
1. Enter organization with special characters (if valid)
2. Enter very long organization strings
3. Enter organization with unicode characters
4. Verify handling is correct

#### 5.5 Test Concurrent Modifications
1. Open Project Properties dialog
2. In another window/process, modify organization via API/config file (if possible)
3. Verify IDE handles concurrent modifications correctly
4. Verify Language Server receives consistent state

#### 5.6 Test Migration Scenarios
1. Test with old project that has global organization but no folder config:
   - Verify migration logic works (handled by LS)
   - Verify `OrgMigratedFromGlobalConfig` flag is set
   - Verify organization is properly migrated to folder config

### Expected Results
- Edge cases are handled gracefully
- No crashes or unhandled exceptions
- Error messages are clear and helpful
- Fallback behavior works in all scenarios

---

## Test Case 6: Integration with Scans

### Objective
Verify organization settings are correctly used during Snyk scans.

### Prerequisites
- Eclipse IDE with Snyk plugin
- Authenticated Snyk account
- Test project with vulnerabilities
- Multiple organizations available

### Steps

#### 6.1 Test Scan with Auto-Detect Enabled
1. Enable auto-detect organization
2. Click **"Apply"**
3. Trigger a Snyk scan (manual or automatic)
4. Verify scan uses `autoDeterminedOrg` from Language Server
5. Verify scan results show correct organization
6. Verify issues are associated with correct organization

#### 6.2 Test Scan with Manual Organization
1. Disable auto-detect
2. Enter specific organization ID
3. Click **"Apply"**
4. Trigger a Snyk scan
5. Verify scan uses manually entered organization
6. Verify scan results show correct organization

#### 6.3 Test Organization Change During Active Scan
1. Start a scan
2. While scan is running, change organization settings
3. Click **"Apply"**
4. Verify scan behavior (may continue with old org or restart)
5. Verify new scans use updated organization

#### 6.4 Test Scan with Invalid Organization
1. Enter invalid/non-existent organization ID
2. Click **"Apply"**
3. Trigger a Snyk scan
4. Verify appropriate error handling
5. Verify error message is clear and actionable

### Expected Results
- Scans use correct organization based on settings
- Organization changes are reflected in subsequent scans
- Error handling for invalid organizations works correctly
- Scan results correctly reflect organization settings

---

## Test Case 7: Migration Testing

### Objective
Verify the extension correctly handles migrated folder configs received from Language Server and properly displays/uses migrated organization settings.

**Note**: Migration logic itself is handled by Language Server. This test case verifies the extension correctly handles the migrated state.

### Prerequisites
- Eclipse IDE with Snyk plugin
- Test project with existing organization setting
- Language Server initialized
- Access to Language Server config directory (for resetting migration state)
- `jq` and `sponge` tools installed (for resetting migration state)

### Steps

#### 7.1 Test Receiving Migrated Folder Config from LS
1. Open a project that has an organization setting but no folder config
2. Verify Language Server initializes
3. Verify extension receives folder config from LS via `$/snyk.folderConfig` notification
4. Verify migrated folder config is handled correctly:
   - Check that `orgMigratedFromGlobalConfig` flag is received from LS
   - Verify `preferredOrg` or `autoDeterminedOrg` is populated based on migration
   - Verify `orgSetByUser` is set correctly based on migration
   - Verify Project Property Page reflects the migrated state

#### 7.2 Test Display of Migrated Organization Settings
1. With migrated folder config received from LS:
   - Verify **"Auto-select organization"** checkbox state matches `!orgSetByUser`
   - Verify **"Organization"** field shows correct value:
     - If `orgSetByUser = false`: Shows `autoDeterminedOrg` (disabled)
     - If `orgSetByUser = true`: Shows `preferredOrg` (enabled)
2. Verify settings are saved correctly in project preferences

#### 7.3 Test Migration State Reset (for re-testing)
1. Locate Language Server config directory:
   - Typically in: `~/.config/snyk/` or similar location
   - Look for config file: `ls-config-Eclipse Platform` or similar
2. To reset folder config back to unmigrated state, use:
   ```bash
   jq '.INTERNAL_LS_CONFIG |= (fromjson | .folderConfigs |= map_values(del(.preferredOrg, .orgMigratedFromGlobalConfig, .orgSetByUser, .autoDeterminedOrg)) | tostring)' 'ls-config-Eclipse Platform' | sponge 'ls-config-Eclipse Platform'
   ```
3. Restart Language Server or Eclipse
4. Verify extension receives unmigrated folder config from LS
5. Verify LS migrates it and sends back migrated config
6. Verify extension handles the re-migrated config correctly

#### 7.4 Test Migration with Default Organization
1. Set organization to user's default organization (or empty) in global preferences
2. Reset migration state (using command from 7.3)
3. Open project and trigger migration (handled by LS)
4. Verify extension receives migrated folder config with:
   - `orgSetByUser = false` (user opted into LDX-Sync)
   - `autoDeterminedOrg` populated from LDX-Sync
   - `orgMigratedFromGlobalConfig = true`
5. Verify Project Property Page reflects auto-select enabled

#### 7.5 Test Migration with Non-Default Organization
1. Set organization to a non-default organization in global preferences
2. Reset migration state
3. Open project and trigger migration (handled by LS)
4. Verify extension receives migrated folder config with:
   - `orgSetByUser = true` (user opted out of LDX-Sync)
   - `preferredOrg` set to the organization value
   - `orgMigratedFromGlobalConfig = true`
5. Verify Project Property Page reflects manual organization mode

#### 7.6 Test Migration with Existing Folder Config
1. Create a project with existing folder config (already migrated)
2. Verify extension receives folder config from LS
3. Verify migration does not run again (handled by LS)
4. Verify existing folder config values are preserved
5. Verify `orgMigratedFromGlobalConfig` remains `true`

#### 7.7 Test Extension Handling of Unmigrated Folder Configs
1. Reset migration state (using command from 7.3)
2. Open project
3. Verify extension receives unmigrated folder config from LS (if LS sends it)
4. Verify extension handles it gracefully:
   - No crashes or errors
   - Extension waits for LS to migrate
   - Extension receives migrated config after LS processes it

### Expected Results
- Extension correctly receives and stores migrated folder configs from LS
- Project Property Page correctly reflects migrated organization state
- Extension handles both migrated and unmigrated folder configs gracefully
- `orgMigratedFromGlobalConfig` flag is correctly stored and used
- Settings persist correctly after migration

---

## Test Case 8: Global Organization Settings

### Objective
Verify global organization setting works correctly and serves as fallback when project-specific organization is empty.

### Prerequisites
- Eclipse IDE with Snyk plugin
- Authenticated Snyk account
- Test project
- Multiple organizations available

### Steps

#### 8.1 Test Global Organization Setting
1. Open **Window → Preferences → Snyk**
2. Navigate to **Advanced options** section
3. Verify **"Organization"** field is present
4. Enter a valid organization ID in the **"Organization"** field
5. Click **"Apply"** or **"OK"**
6. Verify the setting is saved

#### 8.2 Test Global Organization as Fallback
1. Open **Project Properties → Snyk**
2. Uncheck **"Auto-select organization"** checkbox
3. Clear **"Organization"** field (set to empty)
4. Click **"Apply"**
5. Verify Language Server uses global organization as fallback
6. Run a Snyk scan
7. Verify scan uses global organization

#### 8.3 Test Global Organization Enabled/Disabled State
1. Open **Project Properties → Snyk**
2. With auto-detect **enabled** (checkbox checked):
   - Verify **"Organization"** field in Preferences is disabled
3. With auto-detect **disabled** (checkbox unchecked):
   - Verify **"Organization"** field in Preferences is enabled
4. Verify global organization can be set when enabled

#### 8.4 Test Global Organization Persistence
1. Set global organization in Preferences
2. Close and reopen Preferences
3. Verify global organization setting is retained
4. Verify setting persists across Eclipse restarts

#### 8.5 Test Global Organization Change
1. Set global organization to "org-A"
2. Set project organization to empty (with auto-detect disabled)
3. Verify project uses "org-A" as fallback
4. Change global organization to "org-B"
5. Verify project now uses "org-B" as fallback

### Expected Results
- Global organization setting is saved correctly
- Global organization serves as fallback when project organization is empty
- Global organization field enabled/disabled state matches checkbox state
- Global organization persists correctly
- Global organization changes are reflected in scans

---

## Test Case 9: Opting In/Out of Automatic Org Selection (Critical)

### Objective
Verify opting in and out of automatic organization selection works correctly (critical functionality).

### Prerequisites
- Eclipse IDE with Snyk plugin
- Authenticated Snyk account
- Test project with migrated folder config
- Language Server has `autoDeterminedOrg` populated

### Steps

#### 9.1 Test Opting In to Automatic Org Selection
1. Ensure auto-select is currently **disabled** (checkbox unchecked)
2. Ensure project has a preferred org set
3. Open **Project Properties → Snyk**
4. Check **"Auto-select organization"** checkbox
5. Verify **"Organization"** field shows `autoDeterminedOrg` and is disabled
6. Click **"Apply"**
7. Verify folder config sent to Language Server:
   - `OrgSetByUser = false`
   - `PreferredOrg = ""` (cleared)
   - `AutoDeterminedOrg` is preserved (from LS)
8. Verify Language Server uses `AutoDeterminedOrg` for scans

#### 9.2 Test Opting Out of Automatic Org Selection
1. Ensure auto-select is currently **enabled** (checkbox checked)
2. Open **Project Properties → Snyk**
3. Uncheck **"Auto-select organization"** checkbox
4. Verify **"Organization"** field becomes enabled and is cleared
5. Click **"Apply"**
6. Verify folder config sent to Language Server:
   - `OrgSetByUser = true`
   - `PreferredOrg = ""` (remains empty, user can enter value)
   - `AutoDeterminedOrg` is preserved (from LS)
7. Verify Language Server uses global organization as fallback (since preferred org is empty)
8. Verify user can now enter preferred org manually

#### 9.3 Test Opting In - Verify Preferred Org is Cleared
1. Set organization to a specific value
2. Enable auto-select (check checkbox)
3. Click **"Apply"**
4. Verify:
   - `PreferredOrg = ""` in folder config
   - `OrgSetByUser = false`
   - **"Organization"** field shows `autoDeterminedOrg` and is disabled
   - Scans use `AutoDeterminedOrg`

#### 9.4 Test Opting Out - Verify Preferred Org Can Be Set
1. Enable auto-select
2. Disable auto-select (uncheck checkbox)
3. Enter a preferred organization
4. Click **"Apply"**
5. Verify:
   - `PreferredOrg` = entered value
   - `OrgSetByUser = true`
   - **"Organization"** field shows entered value and is enabled
   - Scans use `PreferredOrg`

#### 9.5 Test Opting In/Out Multiple Times
1. Toggle auto-select checkbox multiple times
2. Make various changes
3. Click **"Apply"** after each change
4. Verify:
   - Each state is saved correctly
   - Folder config reflects current checkbox state
   - Language Server receives correct updates
   - No state corruption or errors

### Expected Results
- Opting in to auto-select clears `PreferredOrg` and sets `OrgSetByUser = false`
- Opting out of auto-select sets `OrgSetByUser = true` and allows manual org entry
- `AutoDeterminedOrg` is always preserved (never modified by IDE)
- Language Server uses correct organization based on current setting
- Multiple toggles work correctly without state corruption

---

## Test Case 10: Eclipse Restart Scenarios

### Objective
Verify organization settings persist correctly after Eclipse restart and folder configs are refreshed.

### Prerequisites
- Eclipse IDE with Snyk plugin
- Authenticated Snyk account
- Test project with folder configs
- Multiple organizations available

### Steps

#### 10.1 Test Eclipse Restart - Authenticated User
1. Configure project with specific organization settings:
   - Set preferred org or enable auto-select
   - Click **"Apply"**
2. Close Eclipse completely
3. Reopen Eclipse
4. Open the same project
5. Open **Project Properties → Snyk**
6. Verify:
   - Settings persist correctly (Eclipse preference system)
   - Folder configs are loaded from storage
   - **Every folder config re-fetches `AutoDeterminedOrg` from LDX-Sync**
   - Language Server sends updated folder configs with fresh `AutoDeterminedOrg`
   - Scans use correct organization

#### 10.2 Test Eclipse Restart - Unauthenticated User
1. Ensure user is **not authenticated**
2. Configure organization settings (settings may be editable even when unauthenticated)
3. Close Eclipse
4. Reopen Eclipse
5. Open the same project
6. Open **Project Properties → Snyk**
7. Verify:
   - **No-op**: Use previously saved values for all fields
   - `AutoDeterminedOrg` remains empty (cannot fetch without auth)
   - Folder configs use stored values
   - No errors occur

#### 10.3 Test Eclipse Restart - Then Authenticate
1. While unauthenticated, configure settings
2. Close and reopen Eclipse
3. Authenticate (OAuth2 or Token)
4. Verify:
   - Folder configs are refreshed
   - `AutoDeterminedOrg` is populated from LDX-Sync
   - Settings work correctly after authentication

#### 10.4 Test Eclipse Restart - Multiple Projects
1. Open multiple projects in Eclipse
2. Configure different organization settings for different projects
3. Close Eclipse
4. Reopen Eclipse
5. Open all projects
6. Verify:
   - Each project maintains its own organization settings
   - Each folder config re-fetches `AutoDeterminedOrg`
   - No cross-contamination between projects

### Expected Results
- Settings persist correctly after Eclipse restart (Eclipse preference system)
- Authenticated users: folder configs re-fetch `AutoDeterminedOrg` from LDX-Sync
- Unauthenticated users: use previously saved values (no-op)
- Multiple projects maintain independent settings
- No errors or state corruption after restart

---

## Error Monitoring

Throughout all tests, monitor:

### Eclipse Error Log
- **Window → Show View → Error Log**
- Look for exceptions, errors, or warnings related to:
  - Organization settings
  - Folder config updates
  - Language Server communication
  - Project property page

### Console Output
- Monitor Eclipse console for:
  - `NullPointerException`
  - `IllegalArgumentException`
  - JSON parsing errors
  - Configuration loading errors
  - Language Server communication errors

### Language Server Logs
- Check Language Server output/logs
- Verify folder config updates are received
- Verify organization resolution works correctly

### Common Issues to Watch For
- Settings not persisting
- UI state not matching actual configuration
- Language Server not receiving updates
- Organization fallback not working
- Race conditions in configuration updates
- Memory leaks from event listeners
- **Circular updates between project settings and folder configs**

---

## Regression Testing

Verify existing functionality still works:

### Basic Functionality
- ✅ Basic Snyk scanning (OSS, Code, IaC)
- ✅ Authentication flow (OAuth2 and Token)
- ✅ Issue display and filtering
- ✅ Preferences management (other settings)
- ✅ Language server communication (general)

### Settings Persistence
- ✅ Project-level settings persist across Eclipse restarts
- ✅ Global settings persist across Eclipse restarts
- ✅ Settings survive plugin updates (if applicable)

### UI Components
- ✅ Project Property Page opens/closes correctly
- ✅ Preferences Page opens/closes correctly
- ✅ All other settings sections work as before
- ✅ Snyk view displays correctly
- ✅ Issue tree/filtering works correctly

---

## Test Data Preparation

### Organizations Setup
1. Create/identify test organizations:
   - Organization A: Default organization
   - Organization B: Secondary organization
   - Organization C: Organization with specific projects
   - Organization D: Organization with Code disabled

### Projects Setup
1. Create test projects:
   - Project with no `.settings` folder (uses defaults)
   - Project with `.settings` folder containing organization
   - Project in multiple organizations
   - Project with invalid organization reference

### Authentication Setup
1. Ensure test account has access to multiple organizations
2. Verify LDX-Sync is working (for auto-select)
3. Test with both OAuth2 and Token authentication

---

## Success Criteria

All test cases should pass with:
- ✅ No crashes or unhandled exceptions
- ✅ Settings persist correctly
- ✅ Language Server receives correct configuration
- ✅ Scans use correct organization
- ✅ UI reflects actual configuration state
- ✅ Fallback behavior works correctly
- ✅ No regression in existing functionality
- ✅ **Migration is handled correctly** (Test Case 7 - extension handles migrated folder configs from LS)
- ✅ **Global organization setting works correctly** (Test Case 8)
- ✅ **Opting in/out of automatic org selection works correctly** (Test Case 9 - Critical)
- ✅ **Settings persist correctly after Eclipse restart** (Test Case 10)

---

## Notes

### Eclipse-Specific Considerations
- Settings are accessed via **Project Properties → Snyk** (project-specific) or **Window → Preferences → Snyk** (global)
- Settings are stored in Eclipse preference system (project-scoped or instance-scoped)
- Folder configs are managed per project
- Language Server integration via `WorkspaceDidChangeConfiguration`

### Language Server Integration
- Organization settings are synchronized via `WorkspaceDidChangeConfiguration`
- Language Server sends folder configs via `$/snyk.folderConfig` notification
- `autoDeterminedOrg` is always provided by Language Server (never modified by IDE)
- `preferredOrg` and `orgSetByUser` are set by IDE based on UI state
- Folder configs are stored in `FolderConfigs` singleton

### Migration Notes
- Existing projects with organization settings will be migrated to folder configs
- Migration happens automatically when Language Server initializes (handled by LS)
- `OrgMigratedFromGlobalConfig` flag tracks migration status

### UI Behavior Notes
- Project org field is disabled when auto-detect is enabled (shows `autoDeterminedOrg`)
- Project org field is enabled when auto-detect is disabled (shows `preferredOrg`)
- Global org field is enabled/disabled based on checkbox state
- Special case: If `orgSetByUser = true` and `preferredOrg = ""`, automatically switch to auto-detect mode

