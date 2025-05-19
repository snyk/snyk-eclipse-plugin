
## Eclipse Plugin Release Steps

**Protocol Version Verification**

-   Ensure the Snyk Language Server Protocol version is correct in the plugin. 
  - `REQUIRED_LS_PROTOCOL_VERSION` in `/io.snyk.eclipse.plugin/src/main/java/io/snyk/languageserver/download/LsBinaries.java`
  
**Update Version Numbers**

-   Update the release version in all files that specify the Eclipse plugin version.

-   Do not change values that do not represent the plugin version.


**Update Changelog**

- In the plugin/extension repo, make sure the the Changelog is updated with the correct version to be released and the correct changes in the release.
  - Make sure Early Access are specified correctly for new feature.



**Build Artifacts**

-   Use  `mvnw clean package`  or  `mvnw clean verify`  to build the project and ensure artifacts are created with the correct versions.

-   Verify the plugin JAR file in the  `target/repository/plugins`  folder to confirm the version is correct.


**Testing**

-   Ensure all tests for the latest commit have passed before proceeding with the release.


**Release via GitHub Actions**

- If you want to do a hotfix with a subset of commits from main, create a hotfix branch off the previous release tag.
  - For the hotfix release, cherry pick the commits you want to go into the hotfix release.
  
-   Trigger the release workflow in GitHub Actions.
  -   If this is a hotfix, select the hotfix branch.


**Release Notes**

-   Edit or generate the release notes on GitHub for this release.
  - Its okay to include all items from any intermediate hotfix releases in the release notes.

**Install and Verify Snyk Plugin in Eclipse**

-   Open Eclipse, navigate to  `Help`  →  `Eclipse Marketplace`.

-   Search for "Snyk" and install the latest version of the plugin.

-   Go to  `Eclipse`  →  `About Eclipse`, click  `Installation Details`.

-   Search for "Snyk" and verify that the installed version matches the intended release[6](https://www.clear.rice.edu/comp310/Eclipse/installation.html)[12](https://vaadin.com/docs/v8/framework/installing/installing-eclipse).


**Verify Snyk CLI**

-   Ensure the Snyk CLI is installed, set to the  `stable`  release channel, and automatic updates are enabled.

-   Execute the CLI binary in the terminal and verify that the version matches the intended release.
  - The correct version can be found in the  `#hammerhead-releases`  channel in Slack or in the github cli repo.
     https://github.com/snyk/cli/releases


**Manual Scan**

-   Run a manual scan using the latest version of the Eclipse plugin.


**Publish on Marketplace**


- Login with 1Password

- Go to Edit: https://marketplace.eclipse.org/content/snyk-security/edit

- Scroll to "Solution Version(s)".

- Edit the previous release version.

- Change the version number to the new version number.

- Scroll to the bottom of the page and press "Update".

- Confirm the update took place in Eclipse Marketplace in the IDE.
